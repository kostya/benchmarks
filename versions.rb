cmds = [
['crystal --version', nil, ' | paste -sd " " -'], 
['gcc --version', nil, ' 2>/dev/null | grep version | head -n 1'], 
'nim --version', 
'go version',
'gccgo --version', 
'dmd --version', 
'gdc --version', 
'ldc2 --version', 
'rustc --version',
'scala -version', 
["node -e \"console.log(process.version)\"", 'Nodejs'], 
'pypy --version',
'ruby --version',
'python2 --version',
'python3 --version',
'jruby --version',
'java -version',
'julia --version',
'clang --version',
'mcs --version',
['perl --version', nil, '2>&1 | head -n 2 | tail -n 1'],
'ghc --version',
["tclsh <<< 'puts \"$tcl_version\"'", 'Tcl'],
'jq --version',
'kotlin -version',
'php --version',
['dotnet --version', '.NET Core SDK'],
'elixir --version',
'lua -v',
'luajit -v',
'mlton',
'ocaml --version',
'racket --version',
["clj -e '(print (clojure-version))'", 'Clojure', '2>&1 | tail -n 1'],
'rock --version',
'swift --version',
'fsharp --help | sed -n 1p',
'scheme -q <<< "(scheme-version)"',
'v --version',
'truffleruby --version',
'felix --version',
'd8 --version',
'q --version'
]

def check(name, cmd, tail_cmd = nil)
  lang = cmd.split(' ').first
  `which #{lang} 2>/dev/null`
  exist = $?.exitstatus.to_i == 0

  if exist
    tail_cmd ||= '2>&1 | head -n 1'
    res = `#{cmd} #{tail_cmd}`
    puts "* #{name ? name + ' ' : nil}" + res.sub('Welcome to ', '').sub('This is ', '').gsub(/[:\.]$/, '')
  else
    puts "NOT FOUND: #{lang}"
  end
end

cmds.each do |cmd|
  if cmd.is_a?(Array)
    check(cmd[1], cmd[0], cmd[2])
  else
    check(nil, cmd)
  end
end
