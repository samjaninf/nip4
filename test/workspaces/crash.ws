<?xml version="1.0"?>
<root xmlns="http://www.vips.ecs.soton.ac.uk/nip/9.1.6">
  <Workspace window_x="6" window_y="56" window_width="1022" window_height="605" view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="// private definitions for this workspace&#10;" name="test_image" filename="$HOME/GIT/nip4/test/workspaces/x.ws" major="9" minor="1">

    <Column x="10" y="5" selected="false" sform="false" open="true" next="19" name="B" caption="Sample image">
      <Subcolumn vislevel="3">
        <Row popup="false" name="B1">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="0" window_y="30" window_width="512" window_height="729" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/slanted_oval_vase2.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B18">
          <Rhs vislevel="2" flags="5">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_convert_to_image_item.action B1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>

    <Column x="639" y="5" selected="false" sform="false" open="true" next="12" name="AB" caption="Image / New">
      <Subcolumn vislevel="3">
        <Row popup="false" name="AB1">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1">
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="nwidth">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Image width (pixels)"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="nheight">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Image height (pixels)"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="nbands">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Image bands"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="format_option">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Image format" labelsn="10" labels0="8-bit unsigned int - UCHAR" labels1="8-bit signed int - CHAR" labels2="16-bit unsigned int - USHORT" labels3="16-bit signed int - SHORT" labels4="32-bit unsigned int - UINT" labels5="32-bit signed int - INT" labels6="32-bit float - FLOAT" labels7="64-bit complex - COMPLEX" labels8="64-bit float - DOUBLE" labels9="128-bit complex - DPCOMPLEX" value="4"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="pixel">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Pixel value"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="Vector [42, 255, 42]"/>
                      </Rhs>
                    </Row>
                    <Row name="super">
                      <Rhs vislevel="1" flags="4">
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="interpretation_option">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Interpretation" labelsn="20" labels0="sRGB" labels1="scRGB" labels2="HSV" labels3="XYZ" labels4="Yxy" labels5="Lab" labels6="OKLab" labels7="LCh" labels8="OKLCh" labels9="CMC" labels10="b-w" labels11="LabQ" labels12="LabS" labels13="16-bit RGB" labels14="16-bit mono" labels15="Multiband" labels16="Histogram" labels17="CMYK" labels18="Fourier" labels19="Matrix" value="0"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_new_item.Image_black_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="AB2">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Image_new_item.Image_new_item.Image_new_from_image_item.action AB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="AB3">
          <Rhs vislevel="1" flags="1">
            <iRegion>
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_new_item.Image_new_item.Image_new_item.Image_region_item.Region_item.action AB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="AB4">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_new_item.Image_new_item.Image_new_item.Image_region_item.Mark_item.action AB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="AB5">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_new_item.Image_new_item.Image_new_item.Image_region_item.Arrow_item.action AB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="AB6">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_new_item.Image_new_item.Image_new_item.Image_region_item.HGuide_item.action AB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="AB7">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_new_item.Image_new_item.Image_new_item.Image_region_item.VGuide_item.action AB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="AB8">
          <Rhs vislevel="1" flags="4">
            <iText formula="99"/>
          </Rhs>
        </Row>
        <Row popup="false" name="AB9">
          <Rhs vislevel="2" flags="5">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_convert_to_image_item.action AB8"/>
          </Rhs>
        </Row>
        <Row popup="false" name="AB11">
          <Rhs vislevel="1" flags="1">
            <iRegion>
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_new_item.Image_region_item.Move_item.action B1 AB3"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>

    <Column x="4904" y="5" selected="false" sform="false" open="true" next="49" name="BB" caption="Image / Format">
      <Subcolumn vislevel="3">
        <Row popup="false" name="BB1">
          <Rhs vislevel="0" flags="4">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB28">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.U8_item.action BB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB29">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.U16_item.action BB28"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB30">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.U32_item.action BB29"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB38">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.S8_item.action BB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB39">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.S16_item.action BB38"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB40">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="749" window_y="463" window_width="510" window_height="727"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.S32_item.action BB39"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB41">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.Float_item.action BB30"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB42">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.Double_item.action BB41"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB43">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="753" window_y="463" window_width="734" window_height="727"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.Scmplxitem.action BB42"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB45">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="749" window_y="463" window_width="782" window_height="727"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.Dcmplx_item.action BB43"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB48">
          <Rhs vislevel="2" flags="4">
            <iText formula="if min (BB45 == B1) == 255 then &quot;ok!&quot; else error &quot;cast uchar -&gt;complx != uchar&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>

    <Column x="9816" y="5" selected="false" sform="false" open="true" next="11" name="CB" caption="Image / Join">
      <Subcolumn vislevel="3">
        <Row popup="false" name="CB1">
          <Rhs vislevel="0" flags="4">
            <iImage window_x="4" window_y="53" window_width="547" window_height="729"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="CB2">
          <Rhs vislevel="1" flags="1">
            <iRegion left="103" top="67" width="147" height="122">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region CB1 44 189 147 154"/>
          </Rhs>
        </Row>
        <Row popup="false" name="CB3">
          <Rhs vislevel="1" flags="1">
            <iRegion left="275" top="248" width="136" height="173">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region CB1 275 248 136 141"/>
          </Rhs>
        </Row>
        <Row popup="false" name="CB10">
          <Rhs vislevel="3" flags="7">
            <Colour colour_space="sRGB" value0="114" value1="73" value2="230"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_new_item.Widget_colour_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="CB4">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1">
              <Row name="a">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="b">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="shim">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Spacing" from="0" to="100" value="78.880407124681938"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="bg_colour">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Background colour"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="CB10"/>
                      </Rhs>
                    </Row>
                    <Row name="super">
                      <Rhs vislevel="1" flags="1">
                        <Subcolumn vislevel="0"/>
                        <iText/>
                        <Colour/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="align">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_join_item.Left_right_item.action CB2 CB3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="CB5">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="553" window_y="29" window_width="510" window_height="750"/>
            <Subcolumn vislevel="1">
              <Row name="a">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="b">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="shim">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Spacing" from="0" to="100" value="35.368956743002542"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="bg_colour">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Background colour"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="align">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_join_item.Top_bottom_item.action CB4 CB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="CB6">
          <Rhs vislevel="1" flags="1">
            <iRegion>
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region CB1 19 310 128 132"/>
          </Rhs>
        </Row>
        <Row popup="false" name="CB7">
          <Rhs vislevel="1" flags="1">
            <iRegion>
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region CB1 206 459 130 106"/>
          </Rhs>
        </Row>
        <Row popup="false" name="CB8">
          <Rhs vislevel="1" flags="4">
            <iText formula="[[CB2, CB3],[CB6, CB7]]"/>
          </Rhs>
        </Row>
        <Row popup="false" name="CB9">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="hshim">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Horizontal spacing" from="-100" to="100" value="28.459530026109661"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="vshim">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Vertical spacing" from="-100" to="100" value="25.848563968668415"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="bg_colour">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Background colour"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="halign">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="valign">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_join_item.Array_item.action CB8"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>

    <Column x="10473" y="5" selected="false" sform="false" open="true" next="9" name="DB" caption="Image / Tile">
      <Subcolumn vislevel="3">
        <Row popup="false" name="DB1">
          <Rhs vislevel="2" flags="5">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="DB2">
          <Rhs vislevel="3" flags="7">
            <Subcolumn vislevel="1"/>
            <iText formula="Image_tile_item.Chop_item.action DB1"/>
            <Group/>
          </Rhs>
        </Row>
        <Row popup="false" name="DB3">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="hshim">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Horizontal spacing" from="-100" to="100" value="40"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="vshim">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Vertical spacing" from="-100" to="100" value="40"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="bg_colour">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Background colour"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="halign">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="valign">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_join_item.Array_item.action DB2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="DB4">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1">
              <Row name="default_type">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="across">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Tiles across"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="down">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Tiles down"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="repeat">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Tile type" labelsn="2" labels0="Replicate" labels1="Four-way mirror" value="1"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_tile_item.Replicate_item.action DB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="DB5">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1">
              <Row name="default_type">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="across">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Tiles across"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="1"/>
                      </Rhs>
                    </Row>
                    <Row name="super">
                      <Rhs vislevel="1" flags="4">
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="down">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Tiles down"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="144"/>
                      </Rhs>
                    </Row>
                    <Row name="super">
                      <Rhs vislevel="1" flags="4">
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="repeat">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_tile_item.Replicate_item.action DB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="DB7">
          <Rhs vislevel="3" flags="7">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="tile_height">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Tile height"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="DB1.height"/>
                      </Rhs>
                    </Row>
                    <Row name="super">
                      <Rhs vislevel="1" flags="4">
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="across">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Number of tiles across"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="12"/>
                      </Rhs>
                    </Row>
                    <Row name="super">
                      <Rhs vislevel="1" flags="4">
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="down">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Number of tiles down"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="12"/>
                      </Rhs>
                    </Row>
                    <Row name="super">
                      <Rhs vislevel="1" flags="4">
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_tile_item.Grid_item.action DB5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="DB8">
          <Rhs vislevel="3" flags="7">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="tile_height">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Tile height"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="DB1.height"/>
                      </Rhs>
                    </Row>
                    <Row name="super">
                      <Rhs vislevel="1" flags="4">
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_tile_item.Tranpose3d_item.action DB5"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>

    <Column x="5855" y="5" selected="false" sform="false" open="true" next="7" name="EB" caption="Image / Levels">
      <Subcolumn vislevel="3">
        <Row popup="false" name="EB1">
          <Rhs vislevel="0" flags="4">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="EB3">
          <Rhs vislevel="2" flags="5">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_levels_item.Scale_item.action EB2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="EB2">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="scale">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Scale" from="0.001" to="3" value="1.5998000000000001"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="offset">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_levels_item.Linear_item.action EB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="EB4">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="gamma">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Gamma" from="0.001" to="4" value="2.2693403361344533"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="image_maximum_hint">
                <Rhs vislevel="1" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="im_mx">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Image maximum"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_levels_item.Gamma_item.action EB3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="EB5">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="749" window_y="29" window_width="510" window_height="727"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="b">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="w">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="sp">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="mp">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="hp">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="sa">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Shadow adjust" from="-15" to="15" value="-7.4594594594594597"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ma">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Mid-tone adjust" from="-30" to="30" value="23.675675675675677"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ha">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Highlight adjust" from="-15" to="15" value="-13.135135135135135"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="curve">
                <Rhs vislevel="1" flags="1">
                  <Subcolumn vislevel="0"/>
                  <iText/>
                  <Plot plot_left="0" plot_top="0" plot_mag="100" show_status="false"/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_levels_item.Tone_item.action EB4"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>

    <Column x="6605" y="5" selected="false" sform="false" open="true" next="28" name="FB" caption="Image / Transform">
      <Subcolumn vislevel="3">
        <Row popup="false" name="FB1">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="5" window_y="54" window_width="512" window_height="729" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="FB2">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_transform_item.Rotate_item.Fixed_item.Rot90_item.action FB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="FB3">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="angle">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Angle" from="-180" to="180" value="-59.754098360655732"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="interp">
                <Rhs vislevel="2" flags="5">
                  <Subcolumn vislevel="0"/>
                  <iText/>
                  <Option caption="Interpolate" labelsn="6" labels0="Nearest neighbour" labels1="Bilinear" labels2="Bicubic" labels3="Upsize: reduced halo bicubic (LBB)" labels4="Upsharp: reduced halo bicubic with edge sharpening (Nohalo)" labels5="Upsmooth: quadratic B-splines with jaggy reduction (VSQBS)" value="1"/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_transform_item.Rotate_item.Free_item.action FB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="FB4">
          <Rhs vislevel="1" flags="1">
            <iArrow left="142" top="688" width="36" height="-634">
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Arrow FB1 134 680 44 (-626)"/>
          </Rhs>
        </Row>
        <Row popup="false" name="FB20">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_transform_item.Rotate_item.Straighten_item.action FB4"/>
          </Rhs>
        </Row>
        <Row popup="false" name="FB21">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_transform_item.Flip_item.Left_right_item.action FB20"/>
          </Rhs>
        </Row>
        <Row popup="false" name="FB22">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_transform_item.Flip_item.Top_bottom_item.action FB21"/>
          </Rhs>
        </Row>
        <Row popup="false" name="FB23">
          <Rhs vislevel="3" flags="7">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="xfactor">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Horizontal scale factor"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="0.2"/>
                      </Rhs>
                    </Row>
                    <Row name="super">
                      <Rhs vislevel="1" flags="4">
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="yfactor">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Vertical scale factor"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="0.2"/>
                      </Rhs>
                    </Row>
                    <Row name="super">
                      <Rhs vislevel="1" flags="4">
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="kernel">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Kernel" labelsn="8" labels0="Nearest neighbour" labels1="Linear" labels2="Cubic" labels3="Mitchell" labels4="Lanczos, two lobes" labels5="Lanczos, three lobes" labels6="Magic, 2013" labels7="Magic, 2021" value="1"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_transform_item.Resize_item.Scale_item.action FB22"/>
          </Rhs>
        </Row>
        <Row popup="false" name="FB24">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="which">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="size">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Resize to (pixels)"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="aspect">
                <Rhs vislevel="1" flags="1">
                  <Toggle/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="kernel">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Kernel" labelsn="8" labels0="Nearest neighbour" labels1="Linear" labels2="Cubic" labels3="Mitchell" labels4="Lanczos, two lobes" labels5="Lanczos, three lobes" labels6="Magic, 2013" labels7="Magic, 2021" value="4"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_transform_item.Resize_item.Size_item.action FB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="FB25">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_transform_item.Resize_item.Size_within_item.action FB24"/>
          </Rhs>
        </Row>
        <Row popup="false" name="FB27">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="nwidth">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="New width (pixels)"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="200"/>
                      </Rhs>
                    </Row>
                    <Row name="super">
                      <Rhs vislevel="1" flags="4">
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="nheight">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="New height (pixels)"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="400"/>
                      </Rhs>
                    </Row>
                    <Row name="super">
                      <Rhs vislevel="1" flags="4">
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="bgcolour">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Background colour"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="Vector [128, 0, 0]"/>
                      </Rhs>
                    </Row>
                    <Row name="super">
                      <Rhs vislevel="1" flags="4">
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="position">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Position" labelsn="10" labels0="North-west" labels1="North" labels2="North-east" labels3="West" labels4="Centre" labels5="East" labels6="South-west" labels7="South" labels8="South-east" labels9="Specify in pixels" value="2"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="left">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Pixels from left"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="top">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Pixels from top"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_transform_item.Resize_item.Resize_canvas_item.action FB25"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>



  </Workspace>
</root>
