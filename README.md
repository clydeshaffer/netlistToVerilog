# netlistToVerilog

This project converts a netlist and a partslist exported from an Eagle schematic into some hopefully valid Verilog.

Some assembly is required after conversion. Namely you'll need to import modules with the same names as your ICs that have the same functionality.

Also before conversion it's probably a good idea to do some search/replace in your netlist and partslist files to make sure part names, pin names, and net names don't contain illegal Verilog syntax.