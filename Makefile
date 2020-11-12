define FIFTHHTML_PROLOGUE
<!DOCTYPE html>
<html lang=en>
<head>
	<meta charset="UTF-8">
	<title>Fifth... so far.</title>
	<link rel="stylesheet" href="style.css">
</head>
<body>
endef
export FIFTHHTML_PROLOGUE

define FIFTHHTML_EPILOGUE
</body>
</html>
endef
export FIFTHHTML_EPILOGUE

fifthtour: README.md
	echo "$$FIFTHHTML_PROLOGUE" >  fifthtour.html
	md2html README.md >> fifthtour.html
	echo "$$FIFTHHTML_EPILOGUE" >> fifthtour.html
