IO.puts "iEx starting in "
pwd()
IO.puts "iEx starting on "
IO.puts Node.self

IEx.configure(
  colors: [enabled: true],
  alive_prompt: [
    "\e[G",
    "(%node)",
    "%prefix",
    "<d1>",
  ] |> IO.ANSI.format |> IO.chardata_to_string
)