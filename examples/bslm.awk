#!/usr/bin/env nix-shell
#!nix-shell -i "gawk -f" -p gawk

BEGIN {
  xmin = 1e10
  xmax = -1e10
  ymin = 1e10
  ymax = -1e10
  zmin = 1e10
  zmax = -1e10
}

{
  i = $1

  x[i] = $15
  if ($15 < xmin)
    xmin = $15
  if ($15 > xmax)
    xmax = $15

  y[i] = $16
  if ($16 < ymin)
    ymin = $16
  if ($16 > ymax)
    ymax = $16

  z[i] = $17
  if ($17 < zmin)
    zmin = $17
  if ($17 > zmax)
    zmax = $17
}

END {
  for (i in x) {
    print "upsert {"
    print "  iden: " i
    print "  type: 1"
    print "  mask: 15"
    print "  cnts: 1"
    print "  posx: " ((x[i] - xmin) / (xmax - xmin))
    print "  posy: " ((y[i] - ymin) / (ymax - ymin))
    print "  posz: " ((z[i] - zmin) / (zmax - zmin))
    print "  size: 0.025"
    print "  colr: 4294967295"
    print "  text: \"Run " i "\""
    print "}"
  }
}
