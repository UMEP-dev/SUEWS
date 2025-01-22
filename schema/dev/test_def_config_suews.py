import unittest
import pandas as pd
from def_config_suews import (
    SUEWSConfig,
    Model,
    ModelControl,
    ModelPhysics,
    Site,
    SiteProperties,
    LUMPSParams,
    SPARTACUSParams,
    Conductance,
    IrrigationParams,
    WeeklyProfile,
    HourlyProfile,
    AnthropogenicEmissions,
    AnthropogenicHeat,
    CO2Params,
    SnowParams,
    LandCover,
    PavedProperties,
    BuildingProperties,
    DectrProperties,
    EvetrProperties,
    SurfaceProperties,
    VegetatedSurfaceProperties,
    NonVegetatedSurfaceProperties,
    BaresoilProperties,
    WaterProperties,
    VerticalLayers,
    BuildingLayer,
    ThermalLayer,
    InitialStates,
    DayProfile,
    SurfaceType,
    LAIParams,
    LAIPowerCoefficients,
    StorageDrainParams,
    WaterDistribution,
)


class TestToDataFrameMethods(unittest.TestCase):
    def setUp(self):

        # Initialize a sample SUEWSConfig object with test data
        self.config = SUEWSConfig(
            name="TestConfig",
            description="Test configuration for to_df_state methods",
            model=Model(
                control=ModelControl(
                    tstep=60,
                    forcing_file="forcing.dat",
                    output_file="output.dat",
                    daylightsaving_method=1,
                    diagnose=0,
                ),
                physics=ModelPhysics(
                    netradiationmethod=2,
                    emissionsmethod=3,
                    storageheatmethod=1,
                    ohmincqf=0,
                    roughlenmommethod=2,
                    roughlenheatmethod=2,
                    stabilitymethod=1,
                    smdmethod=0,
                    waterusemethod=1,
                    diagmethod=0,
                    faimethod=1,
                    localclimatemethod=2,
                    snowuse=1,
                ),
            ),
            site=[
                Site(
                    name="TestSite",
                    gridiv=1,
                    properties=SiteProperties(
                        lat=51.4545,
                        lng=-0.9781,
                        alt=100,
                        timezone=0,
                        surfacearea=50.0,
                        z=10.0,
                        z0m_in=0.1,
                        zdm_in=0.2,
                        pipecapacity=1000.0,
                        runofftowater=0.5,
                        narp_trans_site=1.0,
                        lumps=LUMPSParams(
                            raincover=0.3, rainmaxres=0.4, drainrt=0.2, veg_type=1
                        ),
                        spartacus=SPARTACUSParams(
                            air_ext_lw=0.1,
                            air_ext_sw=0.2,
                            air_ssa_lw=0.3,
                            air_ssa_sw=0.4,
                            ground_albedo_dir_mult_fact=0.5,
                            n_stream_lw_urban=10,
                            n_stream_sw_urban=10,
                            n_vegetation_region_urban=5,
                            sw_dn_direct_frac=0.6,
                            use_sw_direct_albedo=0.7,
                            veg_contact_fraction_const=0.8,
                            veg_fsd_const=0.9,
                            veg_ssa_lw=1.0,
                            veg_ssa_sw=1.1,
                        ),
                        conductance=Conductance(
                            g_max=0.5,
                            g_k=0.6,
                            g_q_base=0.7,
                            g_q_shape=0.8,
                            g_t=0.9,
                            g_sm=1.0,
                            kmax=1.1,
                            gsmodel=2,
                            s1=0.2,
                            s2=0.3,
                            tl=15.0,
                            th=25.0,
                        ),
                        irrigation=IrrigationParams(
                            h_maintain=0.1,
                            faut=0.2,
                            ie_start=6.0,
                            ie_end=18.0,
                            internalwateruse_h=0.3,
                            daywatper=WeeklyProfile(
                                monday=0.1,
                                tuesday=0.2,
                                wednesday=0.3,
                                thursday=0.4,
                                friday=0.5,
                                saturday=0.6,
                                sunday=0.7,
                            ),
                            daywat=WeeklyProfile(
                                monday=0.1,
                                tuesday=0.2,
                                wednesday=0.3,
                                thursday=0.4,
                                friday=0.5,
                                saturday=0.6,
                                sunday=0.7,
                            ),
                            wuprofa_24hr=HourlyProfile(
                                working_day={str(i): 0.1 for i in range(1, 25)},
                                holiday={str(i): 0.2 for i in range(1, 25)},
                            ),
                            wuprofm_24hr=HourlyProfile(
                                working_day={str(i): 0.3 for i in range(1, 25)},
                                holiday={str(i): 0.4 for i in range(1, 25)},
                            ),
                        ),
                        anthropogenic_emissions=AnthropogenicEmissions(
                            startdls=0.0,
                            enddls=24.0,
                            heat=AnthropogenicHeat(
                                qf0_beu=DayProfile(working_day=0.1, holiday=0.2),
                                qf_a=DayProfile(working_day=0.3, holiday=0.4),
                                qf_b=DayProfile(working_day=0.5, holiday=0.6),
                                qf_c=DayProfile(working_day=0.7, holiday=0.8),
                                baset_cooling=DayProfile(working_day=0.9, holiday=1.0),
                                baset_heating=DayProfile(working_day=1.1, holiday=1.2),
                                ah_min=DayProfile(working_day=1.3, holiday=1.4),
                                ah_slope_cooling=DayProfile(
                                    working_day=1.5, holiday=1.6
                                ),
                                ah_slope_heating=DayProfile(
                                    working_day=1.7, holiday=1.8
                                ),
                                ahprof_24hr=HourlyProfile(
                                    working_day={str(i): 0.1 for i in range(1, 25)},
                                    holiday={str(i): 0.2 for i in range(1, 25)},
                                ),
                                popdensdaytime=DayProfile(
                                    working_day=1000.0, holiday=800.0
                                ),
                                popdensnighttime=500.0,
                                popprof_24hr=HourlyProfile(
                                    working_day={str(i): 0.1 for i in range(1, 25)},
                                    holiday={str(i): 0.2 for i in range(1, 25)},
                                ),
                            ),
                            co2=CO2Params(
                                co2pointsource=0.01,
                                ef_umolco2perj=0.02,
                                enef_v_jkm=0.03,
                                fcef_v_kgkm=DayProfile(working_day=0.04, holiday=0.05),
                                frfossilfuel_heat=0.06,
                                frfossilfuel_nonheat=0.07,
                                maxfcmetab=0.08,
                                maxqfmetab=0.09,
                                minfcmetab=0.1,
                                minqfmetab=0.11,
                                trafficrate=DayProfile(working_day=100.0, holiday=80.0),
                                trafficunits=1.0,
                                traffprof_24hr=HourlyProfile(
                                    working_day={str(i): 0.1 for i in range(1, 25)},
                                    holiday={str(i): 0.2 for i in range(1, 25)},
                                ),
                                humactivity_24hr=HourlyProfile(
                                    working_day={str(i): 0.3 for i in range(1, 25)},
                                    holiday={str(i): 0.4 for i in range(1, 25)},
                                ),
                            ),
                        ),
                        snow=SnowParams(
                            crwmax=0.5,
                            crwmin=0.3,
                            narp_emis_snow=1.0,
                            preciplimit=10.0,
                            preciplimitalb=0.2,
                            snowalbmax=0.9,
                            snowalbmin=0.1,
                            snowdensmin=0.2,
                            snowdensmax=0.8,
                            snowlimbldg=0.3,
                            snowlimpaved=0.4,
                            snowprof_24hr=HourlyProfile(
                                working_day={str(i): 0.1 for i in range(1, 25)},
                                holiday={str(i): 0.2 for i in range(1, 25)},
                            ),
                            tau_a=0.1,
                            tau_f=0.2,
                            tau_r=0.3,
                            tempmeltfact=0.4,
                            radmeltfact=0.5,
                        ),
                        land_cover=LandCover(
                            paved=PavedProperties(
                                waterdist=WaterDistribution(
                                    to_bldgs=1/7,
                                    to_dectr=1/7,
                                    to_evetr=1/7,
                                    to_grass=1/7,
                                    to_bsoil=1/7,
                                    to_water=1/7,
                                    to_runoff=1/7,
                                ),
                            ),
                            bldgs=BuildingProperties(
                                waterdist=WaterDistribution(
                                    to_paved=1/7,
                                    to_dectr=1/7,
                                    to_evetr=1/7,
                                    to_grass=1/7,
                                    to_bsoil=1/7,
                                    to_water=1/7,
                                    to_runoff=1/7,
                                ),
                            ),
                            dectr=DectrProperties(
                                waterdist=WaterDistribution(
                                    to_paved=1/7,
                                    to_bldgs=1/7,
                                    to_evetr=1/7,
                                    to_grass=1/7,
                                    to_bsoil=1/7,
                                    to_water=1/7,
                                    to_soilstore=1/7,
                                ),
                            ),
                            evetr=EvetrProperties(
                                waterdist=WaterDistribution(
                                    to_paved=1/7,
                                    to_bldgs=1/7,
                                    to_dectr=1/7,
                                    to_grass=1/7,
                                    to_bsoil=1/7,
                                    to_water=1/7,
                                    to_soilstore=1/7,
                                ),
                            ),
                            grass=VegetatedSurfaceProperties(
                                waterdist=WaterDistribution(
                                    to_paved=1/7,
                                    to_bldgs=1/7,
                                    to_dectr=1/7,
                                    to_evetr=1/7,
                                    to_bsoil=1/7,
                                    to_water=1/7,
                                    to_soilstore=1/7,
                                ),
                            ),
                            bsoil=BaresoilProperties(
                                waterdist=WaterDistribution(
                                    to_paved=1/7,
                                    to_bldgs=1/7,
                                    to_dectr=1/7,
                                    to_evetr=1/7,
                                    to_grass=1/7,
                                    to_water=1/7,
                                    to_soilstore=1/7,
                                ),
                            ),
                            water=WaterProperties(),
                        ),
                        vertical_layers=VerticalLayers(
                            nlayer=2,
                            height=[0.0, 10.0, 20.0],
                            veg_frac=[0.3, 0.5],
                            veg_scale=[1.0, 1.5],
                            building_frac=[0.4, 0.6],
                            building_scale=[1.2, 1.8],
                            roofs=[
                                BuildingLayer(),
                                BuildingLayer(),
                            ],
                            walls=[
                                BuildingLayer(),
                                BuildingLayer(),
                            ],
                        ),
                    ),
                    initial_states=InitialStates(),
                )
            ],
        )

    def test_to_df_state_new_matches_original(self):
        """Test that to_df_state_new produces the same output as to_df_state"""
        # load yaml file
        # Load YAML config
        import yaml
        with open("./config-suews.yml", "r") as file:
            yaml_config = yaml.safe_load(file)
        suews_config = SUEWSConfig(**yaml_config[0])
        original_df = suews_config.to_df_state()
        new_df = suews_config.to_df_state_new()
        # Get the column differences
        original_cols = set(original_df.columns)
        new_cols = set(new_df.columns)

        print("\nColumns only in original DataFrame:")
        print(sorted(original_cols - new_cols))

        print("\nColumns only in new DataFrame:")
        print(sorted(new_cols - original_cols))

        print("\nTotal columns in original:", len(original_cols))
        print("Total columns in new:", len(new_cols))

        # # check nan values in the two dataframes
        # print(original_df.isnull().sum())
        # print(new_df.isnull().sum())
        # if any, print the columns with nan values
        print("Columns with nan values in original:")
        print(original_df.columns[original_df.isnull().any()])
        print("Columns with nan values in new:")
        print(new_df.columns[new_df.isnull().any()])
        pd.testing.assert_frame_equal(original_df, new_df)


if __name__ == "__main__":
    unittest.main()
