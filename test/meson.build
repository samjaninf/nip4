should_pass_tests = [
  'test_colour.ws',
  'test_filter.ws',
  'test_histogram.ws',
  'test_image.ws',
  'test_magick.ws',
  'test_matrix.ws',
  'test_region.ws',

]

should_fail_tests = [
  'test_fail.ws',
]

# all the demo workspaces, except frame and register, since they use a
# non-free plugin
demos = [
  #'1_point_mosaic/1pt_mosaic.ws',
  #'2_point_mosaic/2pts_mosaic.ws',
  #'businesscard/businesscard.ws',
  #'clone/clone.ws',
  #'logo/logo2.ws',
  #'manual_balance/manual_balance.ws',
  #'overlays_and_blending/overlay_blend.ws',
]

# scripting language tests
snip_tests = [
  'test_convert.def',
  'test_generate.def',
  'test_list.def',
  'test_object.def',
  'test_predicate.def',
  'test_snip.def',
  'test_stdenv.def',
  'test_types.def',
]

foreach name : should_pass_tests
  file = files('workspaces/' + name)
  test(name, nip4_snip,
    args: ['--test', '--workspace', file],
  )
  test(name, nip4_snip,
    args: ['--test', '--workspace', file],
    env: ['VIPS_NOVECTOR=1']
  )
endforeach

foreach name : should_fail_tests
  file = files('workspaces/' + name)
  test(name, nip4_snip,
    should_fail: true,
    args: ['--test', '--workspace', file],
  )
  test(name, nip4_snip,
    should_fail: true,
    args: ['--test', '--workspace', file],
    env: ['VIPS_NOVECTOR=1'],
  )
endforeach

foreach name : snip_tests
  test(name, nip4_snip,
    args: [files('snip/' + name)]
  )
endforeach

foreach name : demos
  file = files(project_source_root + '/share/nip4/data/examples/' + name)
  test(name, nip4_snip,
    args: ['--test', '--workspace', file],
  )
  # repeat, with the vector system disabled
  test(name, nip4_snip,
    args: ['--test', '--workspace', file],
    env: ['VIPS_NOVECTOR=1'],
  )
endforeach

