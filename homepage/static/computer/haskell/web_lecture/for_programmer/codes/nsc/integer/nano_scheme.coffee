spawn = require('child_process').spawn

module.exports = (robot) -> robot.hear /nsc/i, (msg) ->
	echo = spawn 'echo', [msg.message]
	nsc = spawn 'runghc', ['-ihaskell/nano_scheme', 'haskell/nano_scheme/nsc.hs']
	echo.stdout.on 'data', (data) -> nsc.stdin.write(data)
	nsc.stdout.on 'data', (data) -> msg.send data.toString()
