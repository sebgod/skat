@for %%R in (*.res) do @(
    echo @@ %%R
    call convert_utf8to16 <%%R
)
