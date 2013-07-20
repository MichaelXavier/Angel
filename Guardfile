guard :shell, :all_after_pass => true do
  watch(%r{.*\.cabal$}) do
    run_all_tests
  end

  watch(%r{test/SpecHelper.hs$}) do
    run_all_tests
  end

  def run_all_tests
    ncmd("cabal configure && cabal build && cabal test")
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
 
  def run_tests(mod)
    specfile = "test/#{mod}Spec.hs"
    if File.exists?(specfile)
      files = [specfile]
    else
      files = Dir['test/**/*.hs']
    end

    if package_db = Dir[".cabal-sandbox/*packages.conf.d", "cabal-dev/*packages.conf.d"].first
      package_db_flag = "-package-db #{package_db}"
    end

    ncmd("ghc -isrc -itest #{package_db_flag} -e 'Test.Hspec.hspec spec' #{files.join(' ')}")
  end

  # can we join these? why does run all loop through each file?
  watch(%r{src/(.+)\.hs$}) do |m|
    run_tests(m[1])
  end
 
  watch(%r{test/(.+)Spec\.hs$}) do |m|
    run_tests(m[1])
  end
end
