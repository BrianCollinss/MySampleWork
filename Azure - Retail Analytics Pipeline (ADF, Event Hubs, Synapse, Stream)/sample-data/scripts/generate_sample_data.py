"""Generate the small CSV source files used by the batch ingestion workflow."""

import csv
from pathlib import Path


RAW_DIR = Path(__file__).resolve().parents[1] / "raw"


def write_csv(filename: str, headers: list[str], rows: list[list[str]]) -> None:
    """Write one UTF-8 CSV file into sample-data/raw."""
    target = RAW_DIR / filename
    with target.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow(headers)
        writer.writerows(rows)
    print(f"Wrote {target}")


def main() -> None:
    """Create all sample retail source CSV files."""
    RAW_DIR.mkdir(parents=True, exist_ok=True)
    write_csv(
        "customers.csv",
        ["customer_id", "first_name", "last_name", "email", "signup_date", "loyalty_tier", "city", "state", "country", "marketing_opt_in"],
        [
            ["C001", "Amelia", "Nguyen", "amelia.nguyen@example.com", "2025-10-12", "Gold", "Brisbane", "QLD", "Australia", "Y"],
            ["C002", "Liam", "Patel", "liam.patel@example.com", "2025-11-03", "Silver", "Sydney", "NSW", "Australia", "Y"],
            ["C003", "Olivia", "Thompson", "olivia.thompson@example.com", "2025-11-18", "Bronze", "Melbourne", "VIC", "Australia", "N"],
            ["C004", "Noah", "Evans", "noah.evans@example.com", "2025-12-01", "Gold", "Perth", "WA", "Australia", "Y"],
            ["C005", "Charlotte", "Wilson", "charlotte.wilson@example.com", "2025-12-15", "Silver", "Adelaide", "SA", "Australia", "Y"],
            ["C006", "Jack", "Martin", "jack.martin@example.com", "2026-01-08", "Bronze", "Auckland", "AUK", "New Zealand", "N"],
            ["C007", "Ava", "King", "ava.king@example.com", "2026-01-11", "Gold", "Wellington", "WGN", "New Zealand", "Y"],
            ["C008", "Lucas", "Scott", "lucas.scott@example.com", "2026-01-19", "Silver", "Hobart", "TAS", "Australia", "Y"],
            ["C009", "Grace", "Walker", "grace.walker@example.com", "2026-02-02", "Bronze", "Canberra", "ACT", "Australia", "N"],
            ["C010", "Ethan", "Lee", "ethan.lee@example.com", "2026-02-14", "Silver", "Darwin", "NT", "Australia", "Y"],
        ],
    )
    write_csv(
        "products.csv",
        ["product_id", "product_name", "category", "brand", "unit_price", "unit_cost", "is_active"],
        [
            ["P001", "Everyday Tee", "Apparel", "Northwind Threads", "39.99", "14.50", "Y"],
            ["P002", "Crew Socks 3-Pack", "Accessories", "Northwind Threads", "19.99", "6.25", "Y"],
            ["P003", "Insulated Bottle", "Home", "Blue Gum Living", "59.95", "22.10", "Y"],
            ["P004", "Yoga Mat", "Fitness", "Blue Gum Living", "24.50", "9.30", "Y"],
            ["P005", "Travel Backpack", "Accessories", "Trail Harbour", "89.00", "41.00", "Y"],
            ["P006", "Coffee Beans 1kg", "Grocery", "Harbour Roast", "14.75", "7.10", "Y"],
            ["P007", "Noise-Cancelling Headphones", "Electronics", "Signal Works", "129.00", "78.00", "Y"],
            ["P008", "Notebook Twin Pack", "Stationery", "Paper Lane", "9.99", "3.40", "Y"],
            ["P009", "Desk Lamp", "Home", "Paper Lane", "44.00", "18.50", "Y"],
            ["P010", "Linen Sheet Set", "Home", "Blue Gum Living", "74.50", "31.20", "Y"],
        ],
    )
    write_csv(
        "orders.csv",
        ["order_id", "customer_id", "order_date", "order_status", "payment_method", "shipping_country", "shipping_state", "total_amount"],
        [
            ["O1001", "C001", "2026-04-01", "Submitted", "Card", "Australia", "QLD", "39.99"],
            ["O1002", "C003", "2026-04-01", "Submitted", "PayPal", "Australia", "VIC", "49.00"],
            ["O1003", "C005", "2026-04-01", "Submitted", "Card", "Australia", "SA", "59.97"],
            ["O1004", "C007", "2026-04-01", "Submitted", "Card", "New Zealand", "WGN", "129.00"],
            ["O1005", "C002", "2026-04-02", "Submitted", "Card", "Australia", "NSW", "59.95"],
            ["O1006", "C004", "2026-04-02", "Submitted", "Card", "Australia", "WA", "89.00"],
            ["O1007", "C006", "2026-04-03", "Cancelled", "Card", "New Zealand", "AUK", "14.75"],
            ["O1008", "C008", "2026-04-03", "Submitted", "Card", "Australia", "TAS", "19.98"],
        ],
    )
    write_csv(
        "order_items.csv",
        ["order_item_id", "order_id", "product_id", "quantity", "unit_price", "line_amount"],
        [
            ["OI001", "O1001", "P001", "1", "39.99", "39.99"],
            ["OI002", "O1002", "P004", "2", "24.50", "49.00"],
            ["OI003", "O1003", "P002", "3", "19.99", "59.97"],
            ["OI004", "O1004", "P007", "1", "129.00", "129.00"],
            ["OI005", "O1005", "P003", "1", "59.95", "59.95"],
            ["OI006", "O1006", "P005", "1", "89.00", "89.00"],
            ["OI007", "O1007", "P006", "1", "14.75", "14.75"],
            ["OI008", "O1008", "P008", "2", "9.99", "19.98"],
        ],
    )
    write_csv(
        "campaigns.csv",
        ["campaign_id", "campaign_name", "channel", "start_date", "end_date", "budget", "target_audience"],
        [
            ["CMP001", "Autumn Launch", "email", "2026-03-25", "2026-04-05", "5000", "Loyalty members"],
            ["CMP002", "Home Refresh", "email", "2026-03-28", "2026-04-08", "3500", "Home category browsers"],
            ["CMP003", "Fitness Push", "paid_social", "2026-03-29", "2026-04-10", "4200", "Health-conscious segment"],
            ["CMP004", "Tech Upgrade", "affiliate", "2026-04-01", "2026-04-12", "6000", "Electronics intenders"],
            ["CMP005", "Linen Weekend", "email", "2026-04-01", "2026-04-06", "2800", "Recent home shoppers"],
        ],
    )


if __name__ == "__main__":
    main()
