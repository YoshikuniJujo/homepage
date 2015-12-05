spawn = require('child_process').spawn

module.exports = (robot) -> robot.hear /reverse/i, (msg) ->
	echo = spawn 'echo', [msg.message]
	reverse = spawn 'ghc', ['-e', 'interact $ reverse . head . lines']
	echo.stdout.on 'data', (data) -> reverse.stdin.write(data)
	reverse.stdout.on 'data', (data) -> msg.send data.toString()
