import mdl

def run(filename, outfile):
    """
    This function runs an mdl script
    """
    p = mdl.parseFile(filename)

    if p:
        (commands, symbols) = p
    else:
        print("Parsing failed.")
        return

    view = [0,
            0,
            1];
    ambient = [50,
               50,
               50]
    light = [[0.5,
              0.75,
              1],
             [255,
              255,
              255]]

    color = [0, 0, 0]

    step_3d = 100
    consts = ''
    coords = []
    coords1 = []
    symbols['.white'] = ['constants',
                         {'red': [0.2, 0.5, 0.5],
                          'green': [0.2, 0.5, 0.5],
                          'blue': [0.2, 0.5, 0.5]}]
    reflect = '.white'

    print(symbols)
    constsstr = "__DEF__\n"
    for symbolname, symbolval in symbols.items():
        if symbolval[0] == 'constants':
            constsstr += '__CONST__\n'
            constsstr += symbolname + ' '
            symbolval = symbolval[1];
            for c in symbolval:
                for i in range(3):
                    constsstr += str(symbolval[c][i]) + ' '
            constsstr += '\n'
        elif symbolval[0] == 'knob':
            constsstr += '__KNOB__\n'
            constsstr += symbolname + '\n'
            
    print(constsstr)
    for command in commands:
        print(command)
    commandsstr = "__RUN__\n"
    for command in commands:
        if command['op'] != 'constants':
            commandsstr += '__OP__\n'
            commandsstr += command['op'] + '\n'
            commandsstr += 'args '
            if command.get('args') != None:
                for arg in command['args']:
                    commandsstr += str(arg) + ' '
            commandsstr += '\n'
            if command.get('func') != None:
                commandsstr += 'func ' + str(command['func']) + '\n'
            for key in command:
                if key != 'op' and key != 'args' and key != 'func' and command[key] != None:
                    commandsstr += key + ' '
                    commandsstr += str(command[key]) + '\n'
    print(commandsstr)

    with open(outfile, "w") as f:
        f.write(constsstr)
        f.write(commandsstr)
        f.close()