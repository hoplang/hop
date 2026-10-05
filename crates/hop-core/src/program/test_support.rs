use super::Program;
use crate::document::{Document, DocumentPosition};
use crate::extract_position::extract_position;
use crate::root_contained_file_path::RootContainedFilePath;
use txtar::{Archive, Builder, File};

/// Extracts all position markers from an archive and returns the cleaned
/// archive (with all markers removed) along with the position of each
/// marker found.
pub(super) fn extract_markers_from_archive(archive: &Archive) -> (Archive, Vec<DocumentPosition>) {
    let mut markers = Vec::new();
    let mut builder = Builder::new();

    for file in archive.iter() {
        let document_id = RootContainedFilePath::new(&file.name).unwrap();
        if let Some((document, position)) = extract_position(document_id, &file.content) {
            markers.push(position);
            builder.file(File::new(file.name.clone(), document.as_str().to_string()));
        } else {
            builder.file(file.clone());
        }
    }

    (builder.build(), markers)
}

pub(super) fn program_from_archive(archive: &Archive) -> Program {
    let mut program = Program::new();
    for file in archive.iter() {
        let document_id = RootContainedFilePath::new(&file.name).unwrap();
        let document = Document::new(document_id.clone(), file.content.clone());
        program.update_hop_document(&document_id, document);
    }
    program
}
