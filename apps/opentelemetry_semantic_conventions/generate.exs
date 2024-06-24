semconv_version = "1.26.0"
docker_img_vsn = "0.5.0"
schema_uri = "https://opentelemetry.io/schemas/#{semconv_version}"

# build_dir = System.cmd("mktemp", ["-d"]) |> elem(0) |> String.trim()
build_dir = "#{File.cwd!()}/semtmp"
# File.rm_rf!(build_dir)

# System.cmd("git", [
#   "clone",
#   "https://github.com/open-telemetry/semantic-conventions.git",
#   build_dir
# ])

cwd = File.cwd!()

# File.cd!(build_dir, fn ->
#   System.cmd("git", ["pull"])
#   System.cmd("git", ["checkout", "v#{semconv_version}"])
#   System.cmd("cp", ["-r", "#{build_dir}/docs", "guides"])
#   System.cmd("rm", ["-rf", "#{cwd}/guides/docs"])
# end)

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
  "elixir",
  "/output/"
])

# erlang
# Task.async(fn ->
#   System.cmd("docker", [
#     "run",
#     "--rm",
#     "-v",
#     "#{build_dir}/model:/source",
#     "-v",
#     "#{cwd}/templates:/templates",
#     "-v",
#     "#{cwd}/include:/output",
#     "otel/semconvgen:#{docker_img_vsn}",
#     "--only",
#     kind,
#     "--yaml-root",
#     "/source",
#     "code",
#     "--template",
#     "/templates/semantic_attributes.hrl.j2",
#     "--output",
#     "/output/#{module}.hrl",
#     "-Dmodule=#{module}",
#     "-Dschema_uri=#{schema_uri}"
#   ])
# end)
# end)
# |> List.flatten()
# |> Task.await_many(:timer.minutes(5))

System.cmd("mix", ["format"])
System.cmd("mix", ["docs"])
#
