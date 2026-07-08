use crate::activity_monitor::Total;

//TODO!!!!!!!!!!!!: when enough data is gathered, do the correlations.

//1. deserialize every file in totals directory

//2. for every total of the structure, correlation against the recorded ev of the same day (if no ev, skip).

//3. devide by its time_s, push (name, correlation) tuples to the same Vec.

//4. plot a bar chart with plotly rust
