semconv_version = "1.25.0"
docker_img_vsn = "0.24.0"
schema_uri = "https://opentelemetry.io/schemas/#{semconv_version}"

build_dir = System.cmd("mktemp", ["-d"]) |> elem(0) |> String.trim()

System.cmd("git", [
  "clone",
  "https://github.com/open-telemetry/semantic-conventions.git",
  build_dir
])

cwd = File.cwd!()

File.cd!(build_dir, fn ->
  System.cmd("git", ["checkout", "v#{semconv_version}"])
end)

for kind <- ["metric", "attribute_group", "resource", "span", "event"] do
  module = if kind == "attribute_group", do: "common", else: kind

  # elixir
  Task.async(fn ->
    System.cmd("docker", [
      "run",
      "--rm",
      "-v",
      "#{build_dir}/model:/source",
      "-v",
      "#{cwd}/templates:/templates",
      "-v",
      "#{cwd}/lib:/output",
      "otel/semconvgen:#{docker_img_vsn}",
      "--only",
      kind,
      "--yaml-root",
      "/source",
      "code",
      "--template",
      "/templates/semantic_attributes.ex.j2",
      "--output",
      "/output/#{module}.ex",
      "-Dmodule=#{module}",
      "-Dschema_uri=#{schema_uri}"
    ])
  end)

  # erlang
  Task.async(fn ->
    System.cmd("docker", [
      "run",
      "--rm",
      "-v",
      "#{build_dir}/model:/source",
      "-v",
      "#{cwd}/templates:/templates",
      "-v",
      "#{cwd}/include:/output",
      "otel/semconvgen:#{docker_img_vsn}",
      "--only",
      kind,
      "--yaml-root",
      "/source",
      "code",
      "--template",
      "/templates/semantic_attributes.hrl.j2",
      "--output",
      "/output/#{module}.hrl",
      "-Dmodule=#{module}",
      "-Dschema_uri=#{schema_uri}"
    ])
  end)
end
|> List.flatten()
|> Task.await_many(:timer.minutes(5))

System.cmd("mix", ["format"])
System.cmd("mix", ["docs"])

File.rm_rf!(build_dir)
