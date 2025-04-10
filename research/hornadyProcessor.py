# Quick sketch to generate some CSVs to use for look up tables
#Range	Velocity	Energy	Trajectory	Come Up (MOA)	Come Up (MILS)	Wind Drift	Wind Drift (MOA)	Wind Drift (MILS)

in_file = "hornady_in.csv"
out_file = "hornady_out.csv"

with open(in_file, "r") as f:
    lines = f.readlines()

res_lines = []

for line in lines:
    split = line.split(' ')
    res_lines.append(f"{split[0]},{split[1]},{split[3]}\n")

with open(out_file, 'w') as f:
    f.writelines(res_lines)

res_lines2 = []
for line in lines:
    split = line.split(' ')
    res_lines2.append(f"{split[0]},{split[3]}\n")

out_file2 = "hornady_out2.csv"
with open(out_file2, "w") as f:
    f.writelines(res_lines2)
