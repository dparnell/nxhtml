<?php

$name       = "Joe Smith";
$occupation = "Programmer";
echo <<<EOF

This is a heredoc text-mode section.
For more information talk to $name, your local $occupation.

Thanks!

EOF;

$toprint = <<< HTMLEOF
<!-- heredoc html-mode section -->
<style type="text/css">
.bugfix { color: red; }
</style>
HTMLEOF;
echo strtolower($toprint);

?>
