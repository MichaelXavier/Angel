guard :shell do
  watch(%r{.*\.cabal$}) do
    ncmd("cabal build && cabal test")
  end

  def ncmd(cmd, msg = cmd)
    if system(cmd)
      n msg, "Success!"
    else
      n msg, "Failed"
    end
  end
 
  def run_all_tests
    ncmd("ghc -iAngel -itest -e 'hspec spec' test/**/*.hs test/Spec.hs")
  end

  watch(%r{(Angel/.+)\.hs$}) do |m|
    run_all_tests
  end
 
  watch(%r{test/(.+)Spec\.hs$}) do |m|
    run_all_tests
  end
 
end
