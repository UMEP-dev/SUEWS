# settings.py
from datetime import date
from pydantic import BaseModel, Field, field_validator
from typing import Optional

class UrbanFraction(BaseModel):
    """Land cover fractions for urban site"""
    veg: float = Field(..., ge=0, le=1, description="Fraction vegetated")
    water: float = Field(0.0, ge=0, le=1, description="Fraction water")
    paved: float = Field(..., ge=0, le=1, description="Fraction paved")
    
    @field_validator('*')
    @classmethod
    def validate_fraction(cls, v):
        if not 0 <= v <= 1:
            raise ValueError(f"Fraction must be between 0 and 1, got {v}")
        return v

class SuPySettings(BaseModel):
    """Core SuPy simulation settings"""
    site_id: str = Field(..., pattern=r"^\d{3}$", 
                         description="Three-digit SUEWS site code")
    start_date: date = Field(..., description="Simulation start date")
    end_date: date = Field(..., description="Simulation end date")
    timestep: int = Field(300, ge=60, le=3600, 
                          description="Time step in seconds")
    urban_frac: UrbanFraction = Field(..., 
                                     description="Urban land cover fractions")
    anthrop_heat: float = Field(20, ge=0, le=800, 
                               description="Anthropogenic heat flux (W/m²)")
    met_forcing_file: str = Field(..., 
                                 description="Path or resource ID for met forcing")
    
    # Optional fields for future extension
    lat: Optional[float] = Field(None, ge=-90, le=90, description="Latitude")
    lon: Optional[float] = Field(None, ge=-180, le=180, description="Longitude")
    supy_version: Optional[str] = Field(None, description="SuPy version used")
    
    @field_validator('end_date')
    @classmethod
    def validate_dates(cls, v, info):
        if 'start_date' in info.data and v <= info.data['start_date']:
            raise ValueError('end_date must be after start_date')
        return v
    
    def validate_fractions(self) -> None:
        """Ensure urban fractions sum to 1.0"""
        total = self.urban_frac.veg + self.urban_frac.water + self.urban_frac.paved
        if abs(total - 1.0) > 0.01:
            raise ValueError(f"Urban fractions must sum to 1.0, got {total:.3f}")