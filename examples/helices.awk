#!/usr/bin/env nix-shell
#!nix-shell -i "gawk -f" -p gawk

BEGIN {

  print "reset: false"

  print ""

  print "upsert {"
  print "  iden: 301"
  print "  type: 2"
  print "  mask: 15"
  print "  cnts: 101"
  print "  cnts: 101"
  for (i = 0; i <= 100; ++i)
    print "  posx: " (i / 100)
  for (i = 0; i <= 100; ++i)
    print "  posx: " (i / 100)
  for (i = 0; i <= 100; ++i)
    print "  posy: " (0.5 + 0.4 * cos(4 * 3.1415926 / 100 * i))
  for (i = 0; i <= 100; ++i)
    print "  posy: " (0.5 - 0.4 * sin(4 * 3.1415926 / 100 * i))
  for (i = 0; i <= 100; ++i)
    print "  posz: " (0.5 + 0.4 * sin(4 * 3.1415926 / 100 * i))
  for (i = 0; i <= 100; ++i)
    print "  posz: " (0.5 + 0.4 * cos(4 * 3.1415926 / 100 * i))
  print "  size: 0.05"
  print "  colr: 4278190080"
  print "  text: \"two helices\""
  print "}"


}
