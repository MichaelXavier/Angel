guard :shell do
  watch(%r{.*\.cabal$}) do
    ncmd("cabal build && cabal test")
  end

  def ncmd(cmd, msg = cmd)
    output = `#{cmd}`
    puts output
    summary = output.lines.grep(/examples/).first

    if $?.success?
      n "Build Success!", summary
    else
      n "Failed", summary
    end
  end
 
  def run_all_tests
    ncmd("ghc -isrc -itest -e 'hspec spec' test/**/*.hs test/Spec.hs")
  end

  watch(%r{(Angel/.+)\.hs$}) do |m|
    run_all_tests
  end
 
  watch(%r{test/(.+)Spec\.hs$}) do |m|
    run_all_tests
  end
 
end
