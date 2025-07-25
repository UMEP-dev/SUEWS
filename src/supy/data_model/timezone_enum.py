"""Timezone offset enumeration for SUEWS.

This module defines all valid timezone offsets used globally.
Timezones are expressed as hours offset from UTC.
"""

from enum import Enum


class TimezoneOffset(float, Enum):
    """Valid timezone offsets from UTC in hours.

    These represent all standard timezone offsets currently in use worldwide.
    Fractional hours represent regions with 30-minute or 45-minute offsets.
    """

    # UTC-12:00 to UTC-1:00
    UTC_MINUS_12 = -12.0  # Baker Island Time
    UTC_MINUS_11 = -11.0  # Niue Time, Samoa Standard Time
    UTC_MINUS_10 = -10.0  # Hawaii-Aleutian Standard Time, Cook Island Time
    UTC_MINUS_9_30 = -9.5  # Marquesas Islands Time
    UTC_MINUS_9 = -9.0  # Alaska Standard Time, Gambier Islands Time
    UTC_MINUS_8 = -8.0  # Pacific Standard Time (PST)
    UTC_MINUS_7 = -7.0  # Mountain Standard Time (MST)
    UTC_MINUS_6 = -6.0  # Central Standard Time (CST)
    UTC_MINUS_5 = -5.0  # Eastern Standard Time (EST)
    UTC_MINUS_4 = -4.0  # Atlantic Standard Time (AST)
    UTC_MINUS_3_30 = -3.5  # Newfoundland Standard Time
    UTC_MINUS_3 = -3.0  # Brasília Time, Argentina Time
    UTC_MINUS_2 = -2.0  # South Georgia/South Sandwich Islands Time
    UTC_MINUS_1 = -1.0  # Azores Standard Time, Cape Verde Time

    # UTC+0:00
    UTC = 0.0  # Coordinated Universal Time, Greenwich Mean Time

    # UTC+1:00 to UTC+14:00
    UTC_PLUS_1 = 1.0  # Central European Time (CET), West Africa Time
    UTC_PLUS_2 = 2.0  # Eastern European Time (EET), Central Africa Time
    UTC_PLUS_3 = 3.0  # Moscow Time, East Africa Time
    UTC_PLUS_3_30 = 3.5  # Iran Standard Time
    UTC_PLUS_4 = 4.0  # Gulf Standard Time, Samara Time
    UTC_PLUS_4_30 = 4.5  # Afghanistan Time
    UTC_PLUS_5 = 5.0  # Pakistan Standard Time, Yekaterinburg Time
    UTC_PLUS_5_30 = 5.5  # India Standard Time, Sri Lanka Time
    UTC_PLUS_5_45 = 5.75  # Nepal Time
    UTC_PLUS_6 = 6.0  # Bangladesh Standard Time, Bhutan Time
    UTC_PLUS_6_30 = 6.5  # Myanmar Time, Cocos Islands Time
    UTC_PLUS_7 = 7.0  # Indochina Time, Western Indonesian Time
    UTC_PLUS_8 = 8.0  # China Standard Time, Australian Western Standard Time
    UTC_PLUS_8_30 = 8.5  # North Korea Time (Pyongyang Time)
    UTC_PLUS_8_45 = 8.75  # Australian Central Western Standard Time (unofficial)
    UTC_PLUS_9 = 9.0  # Japan Standard Time, Korea Standard Time
    UTC_PLUS_9_30 = 9.5  # Australian Central Standard Time
    UTC_PLUS_10 = 10.0  # Australian Eastern Standard Time, Chamorro Standard Time
    UTC_PLUS_10_30 = 10.5  # Lord Howe Standard Time
    UTC_PLUS_11 = 11.0  # Solomon Islands Time, Vanuatu Time
    UTC_PLUS_12 = 12.0  # New Zealand Standard Time, Fiji Time
    UTC_PLUS_12_45 = 12.75  # Chatham Standard Time
    UTC_PLUS_13 = 13.0  # Phoenix Islands Time, Tonga Time
    UTC_PLUS_14 = 14.0  # Line Islands Time

    @classmethod
    def _missing_(cls, value):
        """Handle lookup of float values."""
        # Allow lookup by float value
        if isinstance(value, (int, float)):
            for member in cls:
                if (
                    abs(member.value - float(value)) < 0.001
                ):  # Handle floating point comparison
                    return member
        return None

    def __str__(self):
        """Return string representation of timezone offset."""
        hours = int(self.value)
        minutes = int((abs(self.value) - abs(hours)) * 60)

        if minutes == 0:
            return f"UTC{hours:+d}:00"
        else:
            sign = "+" if self.value >= 0 else "-"
            return f"UTC{sign}{abs(hours)}:{minutes:02d}"

    @property
    def hours(self):
        """Return the offset in hours as a float."""
        return self.value

    @property
    def minutes(self):
        """Return the offset in minutes as an integer."""
        return int(self.value * 60)

    @property
    def seconds(self):
        """Return the offset in seconds as an integer."""
        return int(self.value * 3600)
