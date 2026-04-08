"""Small Python helpers that are imported from R via reticulate."""

def summarize_values(values):
    # Materialize the input so we can safely inspect it multiple times.
    values = list(values)

    if not values:
        return {"n": 0, "mean": None, "min": None, "max": None}

    # Return a simple summary dictionary that maps cleanly back into R.
    return {
        "n": len(values),
        "mean": sum(values) / len(values),
        "min": min(values),
        "max": max(values),
    }
