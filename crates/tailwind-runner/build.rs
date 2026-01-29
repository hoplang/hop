use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::Path;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

const TAILWIND_VERSION: &str = "v4.1.13";

// SHA256 checksums for Tailwind CSS v4.1.13 binaries
const CHECKSUMS: &[(&str, &str)] = &[
    (
        "tailwindcss-linux-arm64",
        "c90529475a398adbf3315898721c0f9fe85f434a2b3ea3eafada68867641819a",
    ),
    (
        "tailwindcss-linux-x64",
        "b9ed9f8f640d3323711f9f68608aa266dff3adbc42e867c38ea2d009b973be11",
    ),
    (
        "tailwindcss-macos-arm64",
        "c47681e9948db20026a913a4aca4ee0269b4c0d4ef3f71343cb891dfdc1e97c9",
    ),
    (
        "tailwindcss-macos-x64",
        "c3b230bdbfaa46c94cad8db44da1f82773f10bac54f56fa196c8977d819c09e4",
    ),
    (
        "tailwindcss-windows-x64.exe",
        "ad16a528e13111e5df4e771b4b4981bd4b73e69140fa021f4102f46f02eeb86d",
    ),
];

fn main() -> Result<()> {
    println!("cargo:rerun-if-changed=build.rs");

    let out_dir = env::var("OUT_DIR")?;
    let binaries_dir = Path::new(&out_dir).join("binaries");
    fs::create_dir_all(&binaries_dir)?;

    // Convert checksums to HashMap for easy lookup
    let checksums: HashMap<String, String> = CHECKSUMS
        .iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect();

    let target = env::var("TARGET")?;

    // Download the binary and get its checksum
    let binary_checksum = if target.contains("linux") {
        download_and_compress_binary("linux", "x64", &binaries_dir, &checksums)?
    } else if target.contains("darwin") || target.contains("macos") {
        // Determine architecture for macOS
        if target.contains("aarch64") {
            download_and_compress_binary("macos", "arm64", &binaries_dir, &checksums)?
        } else {
            download_and_compress_binary("macos", "x64", &binaries_dir, &checksums)?
        }
    } else {
        return Err(format!("Unsupported target platform: {}", target).into());
    };

    // Export the checksum as an environment variable for use in lib.rs
    println!("cargo:rustc-env=TAILWIND_CHECKSUM={}", binary_checksum);

    Ok(())
}

fn download_and_compress_binary(
    os: &str,
    arch: &str,
    binaries_dir: &Path,
    checksums: &HashMap<String, String>,
) -> Result<String> {
    let binary_name = format!("tailwindcss-{}", os);
    let compressed_path = binaries_dir.join(&binary_name);

    let download_name = format!("tailwindcss-{}-{}", os, arch);

    // Get expected checksum
    let expected_checksum = checksums
        .get(&download_name)
        .ok_or_else(|| format!("No checksum found for {}", download_name))?;

    if compressed_path.exists() {
        println!("Using cached binary: {}", compressed_path.display());
        return Ok(expected_checksum.clone());
    }

    let url = format!(
        "https://github.com/tailwindlabs/tailwindcss/releases/download/{}/{}",
        TAILWIND_VERSION, download_name
    );

    println!("Downloading Tailwind CSS binary from: {}", url);

    let client = reqwest::blocking::Client::builder()
        .timeout(std::time::Duration::from_secs(90))
        .build()?;
    let response = client.get(&url).send()?;
    if !response.status().is_success() {
        return Err(format!("Failed to download binary: HTTP {}", response.status()).into());
    }

    let binary_data = response.bytes()?;
    println!("Downloaded {} bytes", binary_data.len());

    // Verify checksum
    println!("Verifying checksum...");
    let mut hasher = Sha256::new();
    hasher.update(&binary_data);
    let result = hasher.finalize();
    let actual_checksum = format!("{:x}", result);

    if actual_checksum != *expected_checksum {
        return Err(format!(
            "Checksum verification failed for {}!\nExpected: {}\nActual: {}",
            download_name, expected_checksum, actual_checksum
        )
        .into());
    }
    println!("Checksum verified successfully");

    // Use fast compression in debug, maximum compression in release
    let profile = env::var("PROFILE").unwrap_or_else(|_| "release".to_string());
    let compression_level = if profile == "debug" {
        1 // Fast compression for development
    } else {
        19 // High compression for release builds (good balance of speed and size)
    };

    println!(
        "Compressing binary with zstd (level {})...",
        compression_level
    );
    let compressed = zstd::encode_all(binary_data.as_ref(), compression_level)?;
    println!("Compressed to {} bytes", compressed.len());

    fs::write(&compressed_path, compressed)?;
    println!("Saved compressed binary to: {}", compressed_path.display());

    Ok(expected_checksum.clone())
}
