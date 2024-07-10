semconv_version = "1.26.0"
docker_img_vsn = "0.5.0"
schema_uri = "https://opentelemetry.io/schemas/#{semconv_version}"

# build_dir = System.cmd("mktemp", ["-d"]) |> elem(0) |> String.trim()
build_dir = "#{File.cwd!()}/semtmp"
# File.rm_rf!(build_dir)

cwd = File.cwd!()

#######
# Uncomment the following two sections to setup your env
# #####

# System.cmd("git", [
#   "clone",
#   "https://github.com/open-telemetry/semantic-conventions.git",
#   build_dir
# ])

# File.cd!(build_dir, fn ->
#   System.cmd("git", ["pull"])
#   System.cmd("git", ["checkout", "v#{semconv_version}"])
#   System.cmd("cp", ["-r", "#{build_dir}/docs", "guides"])
#   System.cmd("rm", ["-rf", "#{cwd}/guides/docs"])
# end)

########
# Delete all generated files before generating new versions.
########

# elixir
System.cmd("docker", [
  "run",
  # "--rm",
  "-v",
  "#{build_dir}:/source",
  "-v",
  "#{cwd}/templates:/weaver/templates",
  "-v",
  "#{cwd}/lib:/output",
  "local-weaver",
  # "otel/weaver:#{docker_img_vsn}",
  "registry",
  "generate",
  "--registry=/source/model",
  "--templates=/weaver/templates",
  "--param",
  "stability=stable",
  "elixir",
  "/output/"
])

System.cmd("docker", [
  "run",
  # "--rm",
  "-v",
  "#{build_dir}:/source",
  "-v",
  "#{cwd}/templates:/weaver/templates",
  "-v",
  "#{cwd}/lib/incubating:/output",
  "local-weaver",
  # "otel/weaver:#{docker_img_vsn}",
  "registry",
  "generate",
  "--registry=/source/model",
  "--templates=/weaver/templates",
  "--param",
  "stability=experimental",
  "elixir",
  "/output/"
])

# erlang

System.cmd("docker", [
  "run",
  # "--rm",
  "-v",
  "#{build_dir}:/source",
  "-v",
  "#{cwd}/templates:/weaver/templates",
  "-v",
  "#{cwd}/include:/output",
  "local-weaver",
  # "otel/weaver:#{docker_img_vsn}",
  "registry",
  "generate",
  "--registry=/source/model",
  "--templates=/weaver/templates",
  "--param",
  "stability=stable",
  "erlang",
  "/output/"
])

System.cmd("docker", [
  "run",
  # "--rm",
  "-v",
  "#{build_dir}:/source",
  "-v",
  "#{cwd}/templates:/weaver/templates",
  "-v",
  "#{cwd}/include/incubating:/output",
  "local-weaver",
  # "otel/weaver:#{docker_img_vsn}",
  "registry",
  "generate",
  "--registry=/source/model",
  "--templates=/weaver/templates",
  "--param",
  "stability=experimental",
  "erlang",
  "/output/"
])

System.cmd("mix", ["format"])
System.cmd("mix", ["docs"])
