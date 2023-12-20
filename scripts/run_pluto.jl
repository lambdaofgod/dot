using Pkg
Pkg.add("Pluto")
Pkg.add("ArgParse")

using Pluto
using ArgParse

function parse_commandline()
    s = ArgParseSettings()

    @add_arg_table! s begin
        "--project_path"
            arg_type = String
            required = true
            help = "Path to the project."

        "--ip"
            arg_type = String
            required = false
            default = "192.168.1.21"
        "--port"
            arg_type = Int
            required = false
            default = 1234
        "--secret"
            arg_type = String
            required = true
    end

    return parse_args(ARGS, s)
end

args = parse_commandline()

session = Pluto.ServerSession(
    secret=args["secret"],
    options=Pluto.Configuration.from_flat_kwargs(
        host=args["ip"],
        port=args["port"])
)

Pkg.activate(args["project_path"])
Pluto.run(session)
