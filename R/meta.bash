BEGIN {
    most_recent_date = 0
    most_recent_row = ""
}
{
    # Assuming the date is in the 3rd column; adjust `$3` to match your CSV
    date = $17
    # Remove quotes if the date is quoted
    gsub(/"/, "", date)
    # Convert to a sortable timestamp (epoch seconds)
    cmd = "date -d \"" date "\" +%s"
    cmd | getline epoch
    close(cmd)
    # Compare epoch seconds to find the most recent
    if (epoch > most_recent_date) {
        most_recent_date = epoch
        most_recent_row = $0
    }
}
END {
    print most_recent_row
}
