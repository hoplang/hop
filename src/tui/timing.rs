use std::time::Instant;

#[derive(Debug)]
pub struct TimingCollector {
    phases: Vec<(String, u128)>,
    current_phase: Option<(String, Instant)>,
}

impl TimingCollector {
    pub fn new() -> Self {
        Self {
            phases: Vec::new(),
            current_phase: None,
        }
    }

    pub fn start_phase(&mut self, name: &str) {
        // End the current phase if there is one
        if let Some((phase_name, start_time)) = self.current_phase.take() {
            let duration = start_time.elapsed().as_millis();
            self.phases.push((phase_name, duration));
        }

        // Start the new phase
        self.current_phase = Some((name.to_string(), Instant::now()));
    }

    pub fn end_phase(&mut self) {
        if let Some((phase_name, start_time)) = self.current_phase.take() {
            let duration = start_time.elapsed().as_millis();
            self.phases.push((phase_name, duration));
        }
    }

    pub fn format(&mut self) -> String {
        // End any current phase
        self.end_phase();

        // Convert to the format expected by the visualization function
        let timings: Vec<(&str, u128)> = self
            .phases
            .iter()
            .map(|(name, duration)| (name.as_str(), *duration))
            .collect();

        format_timing_visualization(&timings)
    }

    pub fn print(&mut self) {
        print!("{}", self.format());
    }
}

pub fn format_timing_visualization(timings: &[(&str, u128)]) -> String {
    // Constants
    const MAX_BAR_WIDTH: usize = 30;
    const BAR_CHAR: &str = "━";

    if timings.is_empty() {
        return String::new();
    }

    let total_time: u128 = timings.iter().map(|(_, time)| time).sum();
    if total_time == 0 {
        return String::new();
    }

    let mut output = String::new();

    output.push('\n');

    // Print individual timings with bars
    for (name, time) in timings.iter() {
        let bar_width = if total_time > 0 {
            let calculated_width =
                ((*time as f64 / total_time as f64) * MAX_BAR_WIDTH as f64).round() as usize;
            calculated_width.max(1) // Ensure at least 1 bar character
        } else {
            1
        };

        let bar = BAR_CHAR.repeat(bar_width);
        output.push_str(&format!(
            "  {:<20} {:<width$}{:>5}ms\n",
            name,
            bar,
            time,
            width = MAX_BAR_WIDTH
        ));
    }

    output
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};

    fn check(timings: Vec<(&str, u128)>, expect: Expect) {
        let actual = format_timing_visualization(&timings);
        let actual_with_pipes = actual
            .lines()
            .map(|line| format!("|{}", line))
            .collect::<Vec<_>>()
            .join("\n")
            + "\n";
        expect.assert_eq(&actual_with_pipes);
    }

    #[test]
    fn timing_visualization_with_multiple_phases() {
        let timings = vec![
            ("module loading", 10),
            ("compilation", 50),
            ("file rendering", 20),
            ("file writing", 5),
        ];

        check(
            timings,
            expect![[r#"
                |
                |  module loading       ━━━━                             10ms
                |  compilation          ━━━━━━━━━━━━━━━━━━               50ms
                |  file rendering       ━━━━━━━                          20ms
                |  file writing         ━━                                5ms
            "#]],
        );
    }

    #[test]
    fn timing_visualization_with_two_phases() {
        let timings = vec![("module loading", 100), ("compilation", 0)];

        check(
            timings,
            expect![[r#"
                |
                |  module loading       ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━  100ms
                |  compilation          ━                                 0ms
            "#]],
        );
    }
}
