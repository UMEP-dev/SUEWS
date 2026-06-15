from supy import SUEWSSimulation


#sim = SUEWSSimulation("YL_vegetation.yml")
sim = SUEWSSimulation("YL_vegetation_13Apr.yml")
sim.run()
print(sim.results)

sim.save("output_directory/13Apr/veg/")
#sim.save("output_directory/")