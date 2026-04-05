<?xml version="1.0"?>
<root xmlns="http://www.vips.ecs.soton.ac.uk/nip/9.1.0">
  <Workspace window_x="6" window_y="56" window_width="1022" window_height="605" view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="// private definitions for this workspace&#10;" name="test_image" filename="$CWD/test_image.ws" major="9" minor="1">
    <Column x="10" y="5" open="true" selected="false" sform="false" next="19" name="B" caption="Sample image">
      <Subcolumn vislevel="3">
        <Row popup="false" name="B1">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="0" window_y="30" window_width="512" window_height="729" show_status="true" show_convert="true" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$CWD/images/slanted_oval_vase2.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B18">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_convert_to_image_item.action B1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="456" y="5" open="true" selected="false" sform="false" next="12" name="AB" caption="Image / New">
      <Subcolumn vislevel="3">
        <Row popup="false" name="AB1">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
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
                  <Option caption="Interpretation" labelsn="20" labels0="sRGB" labels1="scRGB" labels2="HSV" labels3="XYZ" labels4="Yxy" labels5="Lab" labels6="OKLab" labels7="LCh" labels8="OKLCh" labels9="CMC" labels10="Mono" labels11="LabQ" labels12="LabS" labels13="16-bit RGB" labels14="16-bit mono" labels15="Multiband" labels16="Histogram" labels17="CMYK" labels18="Fourier" labels19="Matrix" value="0"/>
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
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Image_new_item.Image_new_item.Image_new_from_image_item.action AB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="AB3">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false">
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
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_convert_to_image_item.action AB8"/>
          </Rhs>
        </Row>
        <Row popup="false" name="AB11">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_new_item.Image_region_item.Move_item.action B1 AB3"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="4721" y="5" open="true" selected="false" sform="false" next="49" name="BB" caption="Image / Format">
      <Subcolumn vislevel="3">
        <Row popup="false" name="BB1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB28">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.U8_item.action BB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB29">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.U16_item.action BB28"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB30">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.U32_item.action BB29"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB38">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.S8_item.action BB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB39">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.S16_item.action BB38"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB40">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="749" window_y="463" window_width="510" window_height="727" show_status="true" show_convert="true"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.S32_item.action BB39"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB41">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.Float_item.action BB30"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB42">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.Double_item.action BB41"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB43">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="753" window_y="463" window_width="734" window_height="727" show_status="true" show_convert="true"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.Scmplxitem.action BB42"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BB45">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="749" window_y="463" window_width="782" window_height="727" show_status="true" show_convert="true"/>
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
    <Column x="12230" y="5" open="true" selected="false" sform="false" next="11" name="CB" caption="Image / Join">
      <Subcolumn vislevel="3">
        <Row popup="false" name="CB1">
          <Rhs vislevel="0" flags="4">
            <iImage window_x="4" window_y="53" window_width="547" window_height="729" show_status="true" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="CB2">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false" left="103" top="67" width="147" height="122">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region CB1 44 189 147 154"/>
          </Rhs>
        </Row>
        <Row popup="false" name="CB3">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false" left="275" top="248" width="136" height="173">
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
            <iImage show_status="false" show_convert="false"/>
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
                  <iImage show_status="false" show_convert="false"/>
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
            <iImage window_x="553" window_y="29" window_width="510" window_height="750" show_status="true" show_convert="false"/>
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
                  <iImage show_status="false" show_convert="false"/>
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
            <iRegion show_status="false" show_convert="false">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region CB1 19 310 128 132"/>
          </Rhs>
        </Row>
        <Row popup="false" name="CB7">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false">
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
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
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
    <Column x="12900" y="5" open="true" selected="true" sform="false" next="9" name="DB" caption="Image / Tile">
      <Subcolumn vislevel="3">
        <Row popup="false" name="DB1">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
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
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
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
            <iImage show_status="false" show_convert="false"/>
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
                  <iImage show_status="false" show_convert="false"/>
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
            <iImage show_status="false" show_convert="false"/>
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
                  <iImage show_status="false" show_convert="false"/>
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
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
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
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
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
    <Column x="5672" y="5" open="true" selected="false" sform="false" next="7" name="EB" caption="Image / Levels">
      <Subcolumn vislevel="3">
        <Row popup="false" name="EB1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="EB3">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_levels_item.Scale_item.action EB2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="EB2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
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
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
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
            <iImage window_x="749" window_y="29" window_width="510" window_height="727" show_status="true" show_convert="true"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
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
    <Column x="6422" y="5" open="true" selected="false" sform="false" next="28" name="FB" caption="Image / Transform">
      <Subcolumn vislevel="3">
        <Row popup="false" name="FB1">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="5" window_y="54" window_width="512" window_height="729" show_status="true" show_convert="true" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="FB2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_transform_item.Rotate_item.Fixed_item.Rot90_item.action FB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="FB3">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
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
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_transform_item.Rotate_item.Straighten_item.action FB4"/>
          </Rhs>
        </Row>
        <Row popup="false" name="FB21">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_transform_item.Flip_item.Left_right_item.action FB20"/>
          </Rhs>
        </Row>
        <Row popup="false" name="FB22">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_transform_item.Flip_item.Top_bottom_item.action FB21"/>
          </Rhs>
        </Row>
        <Row popup="false" name="FB23">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
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
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
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
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_transform_item.Resize_item.Size_within_item.action FB24"/>
          </Rhs>
        </Row>
        <Row popup="false" name="FB27">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
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
    <Column x="8606" y="5" open="true" selected="false" sform="false" next="25" name="A" caption="Image / Band">
      <Subcolumn vislevel="3">
        <Row popup="false" name="A1">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A3">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="first">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Extract from band"/>
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
              <Row name="number">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Extract this many bands"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="2 "/>
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
            <iText formula="Image_band_item.Extract_item.action A1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A4">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_band_item.Insert_item.action A1 A3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A5">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="0" window_y="30" window_width="512" window_height="729" show_status="true" show_convert="true"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="first">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Delete from band"/>
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
              <Row name="number">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Delete this many bands"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="2"/>
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
            <iText formula="Image_band_item.Delete_item.action A4"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A18">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_band_item.Bandwise_item.action A5 A4"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A23">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_band_item.Band_unfold_item.action A1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A24">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="factor">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Fold factor"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="3"/>
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
            <iText formula="Image_band_item.Band_fold_item.action A23"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A19">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="384" window_y="52" window_width="750" window_height="727" show_status="true" show_convert="true"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="orientation">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Orientation" labelsn="2" labels0="Horizontal" labels1="Vertical" value="0"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_band_item.To_dimension_item.action A18"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A20">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false" left="648" top="0" width="3" height="688">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region A19 558 318 3410 54"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A22">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_band_item.To_bands_item.action A20"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="5040" y="5" open="true" selected="false" sform="false" next="14" name="C" caption="Image / Header">
      <Subcolumn vislevel="3">
        <Row popup="false" name="C1">
          <Rhs vislevel="0" flags="4">
            <iImage window_x="749" window_y="29" window_width="547" window_height="727" show_status="true" show_convert="true"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C2">
          <Rhs vislevel="3" flags="7">
            <Subcolumn vislevel="1"/>
            <iText formula="Image_header_item.Image_get_item.Width_item.action C1"/>
            <Real/>
          </Rhs>
        </Row>
        <Row popup="false" name="C4">
          <Rhs vislevel="3" flags="7">
            <Subcolumn vislevel="1"/>
            <iText formula="Image_header_item.Image_get_item.Yres_item.action C1"/>
            <Real/>
          </Rhs>
        </Row>
        <Row popup="false" name="C3">
          <Rhs vislevel="3" flags="7">
            <Subcolumn vislevel="1"/>
            <iText formula="Image_header_item.Image_get_item.Custom_item.action C1"/>
            <Real/>
          </Rhs>
        </Row>
        <Row popup="false" name="C9">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_header_item.Image_set_meta_item.action C1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C11">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="nxres">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Xres"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="nyres">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Yres"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="nxoff">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Xoffset"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="123"/>
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
              <Row name="nyoff">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Yoffset"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ninterpretation">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Interpretation" labelsn="20" labels0="sRGB" labels1="scRGB" labels2="HSV" labels3="XYZ" labels4="Yxy" labels5="Lab" labels6="OKLab" labels7="LCh" labels8="OKLCh" labels9="CMC" labels10="Mono" labels11="LabQ" labels12="LabS" labels13="16-bit RGB" labels14="16-bit mono" labels15="Multiband" labels16="Histogram" labels17="CMYK" labels18="Fourier" labels19="Matrix" value="0"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_header_item.Image_edit_header_item.action C1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C13">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_cache_item.action C1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="9965" y="5" open="true" selected="false" sform="false" next="20" name="D" caption="Image / Select">
      <Subcolumn vislevel="3">
        <Row popup="false" name="D17">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="337" window_y="172" window_width="547" window_height="727" show_status="true" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D8">
          <Rhs vislevel="1" flags="1">
            <iArrow left="354" top="310" width="-240" height="-247">
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Arrow D17 367 303 (-250) (-238)"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D9">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="131" window_y="236" window_width="510" window_height="727" show_status="true" show_convert="true" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="control">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Make" labelsn="7" labels0="Selection Brighter" labels1="Selection Darker" labels2="Selection Black" labels3="Selection White" labels4="Background Black" labels5="Background White" labels6="Mask" value="1"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="width">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Width" from="0.01" to="1" value="0.20341902313624677"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_select_item.Elipse.action D8"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D10">
          <Rhs vislevel="1" flags="1">
            <iArrow left="295" top="128" width="0" height="0">
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark D17 363 110"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D11">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark D17 71 268"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D12">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark D17 198 530"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D13">
          <Rhs vislevel="1" flags="1">
            <iArrow left="406" top="411" width="0" height="0">
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark D17 298 425"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D14">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
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
              <Row name="c">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="d">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="control">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Make" labelsn="7" labels0="Selection Brighter" labels1="Selection Darker" labels2="Selection Black" labels3="Selection White" labels4="Background Black" labels5="Background White" labels6="Mask" value="1"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_select_item.Tetragon.action D10 D11 D12 D13"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D15">
          <Rhs vislevel="1" flags="1">
            <iText formula="Group [D10, D11, D12]"/>
            <Subcolumn vislevel="0"/>
            <Group/>
          </Rhs>
        </Row>
        <Row popup="false" name="D16">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="pt_list">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="control">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Make" labelsn="7" labels0="Selection Brighter" labels1="Selection Darker" labels2="Selection Black" labels3="Selection White" labels4="Background Black" labels5="Background White" labels6="Mask" value="1"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_select_item.Polygon.action D15"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D18">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="t">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Threshold" from="0" to="255" value="162.18390804597703"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_select_item.Threshold_item.action D17"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D19">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="t">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Percentage of pixels" from="0" to="100" value="14.725274725274726"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_select_item.Threshold_percent_item.action D17"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="9042" y="5" open="true" selected="false" sform="false" next="14" name="F" caption="Image / Alpha">
      <Subcolumn vislevel="3">
        <Row popup="false" name="F1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="F2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_alpha_item.Add_item.action F1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="F3">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_alpha_item.Flatten_item.action F2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="F4">
          <Rhs vislevel="2" flags="4">
            <iText formula="if min (F3 == F1) == 255 then &quot;ok!&quot; else error &quot;erp&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="F5">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_alpha_item.Extract_item.action F2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="F6">
          <Rhs vislevel="1" flags="4">
            <iText formula="if min (F5 == 255) == 255 then &quot;ok!&quot; else error &quot;erp!&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="F7">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_alpha_item.Drop_item.action F2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="F8">
          <Rhs vislevel="1" flags="4">
            <iText formula="if min (F7 == F1) == 255 then &quot;ok!&quot; else error &quot;erp&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="F9">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="72" window_y="72" window_width="570" window_height="727" show_status="true" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_alpha_item.Premultiply_item.action F2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="F11">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_alpha_item.Unpremultiply_item.action F9"/>
          </Rhs>
        </Row>
        <Row popup="false" name="F12">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_alpha_item.Drop_item.action F11"/>
          </Rhs>
        </Row>
        <Row popup="false" name="F13">
          <Rhs vislevel="1" flags="4">
            <iText formula="if min (F12 == F1) == 255 then &quot;ok!&quot; else error &quot;erp&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1164" y="5" open="true" selected="false" sform="false" next="7" name="E" caption="Image / New">
      <Subcolumn vislevel="3">
        <Row popup="false" name="E3">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.SDF_item.Circle_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E4">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.SDF_item.Box_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E5">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.SDF_item.Rounded_box_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E6">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.SDF_item.Line_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Xy_item.action"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1720" y="5" open="true" selected="false" sform="false" next="7" name="G" caption="Image / New">
      <Subcolumn vislevel="3">
        <Row popup="false" name="G1">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Noise_item.Gaussian_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="G2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Noise_item.Fractal_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="G4">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Noise_item.Perlin_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="G5">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Noise_item.Worley_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="G6">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Text_item.action"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="2274" y="5" open="true" selected="false" sform="false" next="4" name="H" caption="Image / New">
      <Subcolumn vislevel="3">
        <Row popup="false" name="H1">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.New_ideal_item.High_low_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="H2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.New_ideal_item.Ring_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="H3">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.New_ideal_item.Band_item.action"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="2898" y="5" open="true" selected="false" sform="false" next="5" name="I" caption="Image / New">
      <Subcolumn vislevel="3">
        <Row popup="false" name="I1">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.New_gaussian_item.High_low_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="I2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.New_gaussian_item.Ring_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="I4">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.New_gaussian_item.Band_item.action"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="3522" y="5" open="true" selected="false" sform="false" next="4" name="J" caption="Image / New">
      <Subcolumn vislevel="3">
        <Row popup="false" name="J1">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.New_butterworth_item.High_low_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="J2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.New_butterworth_item.Ring_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="J3">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.New_butterworth_item.Band_item.action"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="4146" y="5" open="true" selected="false" sform="false" next="14" name="K" caption="Image / New">
      <Subcolumn vislevel="3">
        <Row popup="false" name="K1">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Test_images_item.Eye_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="K2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Test_images_item.Zone_plate.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="K4">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Test_images_item.Frequency_test_chart_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="K5">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Test_images_item.Grey_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="K6">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Test_images_item.Checkerboard_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="K7">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Test_images_item.Grid_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="K8">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Test_images_item.New_CIELAB_slice_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="K9">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Test_images_item.CRT_test_chart_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="K10">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Test_images_item.Greyscale_chart_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="K12">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_new_item.Test_images_item.CMYK_test_chart_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="K13">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="start">
                <Rhs vislevel="3" flags="7">
                  <Colour/>
                  <Subcolumn vislevel="1"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="nstep">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Number of steps"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="2"/>
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
              <Row name="ssize">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Step size"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="psize">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Patch size"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="sepsize">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Separator size"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_new_item.Test_images_item.Colour_atlas_item.action"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="14174" y="5" open="true" selected="false" sform="false" next="11" name="L" caption="Image / Crop">
      <Subcolumn vislevel="3">
        <Row popup="false" name="L1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="L3">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_crop_item.action L1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="L4">
          <Rhs vislevel="1" flags="1">
            <Subcolumn vislevel="0"/>
            <iText formula="Group [L1, L1]"/>
            <Group/>
          </Rhs>
        </Row>
        <Row popup="false" name="L8">
          <Rhs vislevel="3" flags="7">
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <Subcolumn vislevel="0"/>
                  <iText/>
                  <Group/>
                </Rhs>
              </Row>
              <Row name="nleft">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Crop left"/>
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
              <Row name="ntop">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Crop top"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="nwidth">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Crop width"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="nheight">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Crop height"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_crop_item.action L4"/>
            <Group/>
          </Rhs>
        </Row>
        <Row popup="false" name="L9">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="L8.value?0"/>
          </Rhs>
        </Row>
        <Row popup="false" name="L10">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="L8.value?1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="13618" y="5" open="true" selected="false" sform="false" next="19" name="M" caption="Image / SDF">
      <Subcolumn vislevel="3">
        <Row popup="false" name="M10">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_SDF_item.New_item.Circle_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M11">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="255.00000000000043" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1">
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
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
              <Row name="bleft">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Box left"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="64"/>
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
              <Row name="btop">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Box top"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="bright">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Box right"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="bbottom">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Box bottom"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_SDF_item.New_item.Box_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M12">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
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
              <Row name="bleft">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Box left"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="10"/>
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
              <Row name="btop">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Box top"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="10"/>
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
              <Row name="bright">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Box right"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="128"/>
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
              <Row name="bbottom">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Box bottom"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="128"/>
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
              <Row name="tl_radius">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="tr_radius">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="bl_radius">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="br_radius">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Bottom-right corner radius" from="0" to="100" value="67.1875"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_SDF_item.New_item.Rounded_box_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M13">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_SDF_item.New_item.Line_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M14">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false" scale="255.00000000000043" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_SDF_item.Union_item.action M12 M13"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M15">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false" scale="255.00000000000043" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_SDF_item.Intersection_item.action M11 M14"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M16">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false" scale="133.01728699757345" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_SDF_item.Difference_item.action M12 M15"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M17">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="width">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Width" from="0" to="100" value="28.828828828828829"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_SDF_item.Annular_item.action M16"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M18">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_SDF_item.Render_item.action M17"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="14889" y="5" open="true" selected="false" sform="false" next="4" name="N" caption="Image / Trim">
      <Subcolumn vislevel="3">
        <Row popup="false" name="N1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
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
                        <iText formula="N1.width * 2"/>
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
                        <iText formula="N1.height * 2"/>
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
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="position">
                <Rhs vislevel="1" flags="1">
                  <Option/>
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
            <iText formula="Image_transform_item.Resize_item.Resize_canvas_item.action N1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N3">
          <Rhs vislevel="3" flags="7">
            <iRegion show_status="false" show_convert="false">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="1"/>
            <iText formula="Trim_item.action N2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="15435" y="5" open="true" selected="false" sform="false" next="4" name="O" caption="Image / Insert">
      <Subcolumn vislevel="3">
        <Row popup="false" name="O1">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="O2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
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
                        <iText formula="3"/>
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
                        <iText formula="3"/>
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
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_transform_item.Resize_item.Scale_item.action O1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="O3">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_insert_item.action O1 O2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="7040" y="5" open="true" selected="false" sform="false" next="7" name="P" caption="Image / Transform">
      <Subcolumn vislevel="3">
        <Row popup="false" name="P1">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="P6">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_transform_item.Image_perspective_item.action P1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="P4">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap1">
                <Rhs vislevel="1" flags="1">
                  <iArrow>
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap2">
                <Rhs vislevel="1" flags="1">
                  <iArrow>
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap3">
                <Rhs vislevel="1" flags="1">
                  <iArrow>
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap4">
                <Rhs vislevel="1" flags="1">
                  <iArrow>
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="dir">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Select distort direction" labelsn="2" labels0="Distort to points" labels1="Distort to corners" value="0"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="interp">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_transform_item.Image_perspective_item.action P6"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="7557" y="5" open="true" selected="false" sform="false" next="6" name="Q" caption="Image / Transform">
      <Subcolumn vislevel="3">
        <Row popup="false" name="Q1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="Q4">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Object_duplicate_item.action Q1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="Q2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="angle">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Angle" from="-180" to="180" value="22.759322033898229"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="interp">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_transform_item.Rotate_item.Free_item.action Q1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="Q5">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="y">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap1">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="137" top="108" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="bp1">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="352" top="149" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap2">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="429" top="662" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="bp2">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="415" top="779" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="refine">
                <Rhs vislevel="1" flags="1">
                  <Toggle caption="Refine selected tie-points" value="true"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="lock">
                <Rhs vislevel="1" flags="1">
                  <Toggle caption="No resize" value="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="interp">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_transform_item.Match_item.action Q4 Q2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="8043" y="5" open="true" selected="false" sform="false" next="6" name="R" caption="Image / Transform">
      <Subcolumn vislevel="3">
        <Row popup="false" name="R1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="R2">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Object_duplicate_item.action R1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="R3">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap1">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="52" top="59" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap2">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="402" top="132" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap3">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="444" top="597" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap4">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="87" top="680" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="dir">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Select distort direction" labelsn="2" labels0="Distort to points" labels1="Distort to corners" value="0"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="interp">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_transform_item.Image_perspective_item.action R2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="R5">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="y">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap1">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="136" top="110" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap2">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="372" top="295" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap3">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="82" top="654" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap4">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="433" top="663" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="bp1">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="158" top="172" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="bp2">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="333" top="337" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="bp3">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="147" top="638" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="bp4">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="398" top="585" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="interp">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_transform_item.Image_perspective_match_item.action R2 R3"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="9474" y="5" open="true" selected="false" sform="false" next="7" name="S" caption="Image / Alpha">
      <Subcolumn vislevel="3">
        <Row popup="false" name="S1">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="S2">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="S1?1 &lt; 128"/>
          </Rhs>
        </Row>
        <Row popup="false" name="S3">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="S1 ++ S2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="S4">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="S1 + Vector [50, 0, 0]"/>
          </Rhs>
        </Row>
        <Row popup="false" name="S6">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1">
              <Row name="bg">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="fg">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="mode">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Blend" labelsn="29" labels0="Clear" labels1="Source" labels2="Over" labels3="In" labels4="Out" labels5="Atop" labels6="Dest" labels7="Dest over" labels8="Dest in" labels9="Dest out" labels10="Dest atop" labels11="XOR" labels12="Add" labels13="Saturate" labels14="Multiply" labels15="Screen" labels16="Overlay" labels17="Darken" labels18="Lighten" labels19="Colour dodge" labels20="Colour burn" labels21="Hard light" labels22="Soft light" labels23="Difference" labels24="Exclusion" labels25="Hue" labels26="Saturation" labels27="Colour" labels28="Luminosity" value="2"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="space">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="hmove">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Horizontal move by" from="-496" to="496" value="62"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="vmove">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Vertical move by" from="-688" to="688" value="107.5"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="prem">
                <Rhs vislevel="1" flags="1">
                  <Toggle/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_alpha_item.Composite2_item.action S4 S3"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="10696" y="5" open="true" selected="false" sform="false" next="7" name="T" caption="Image / Select">
      <Subcolumn vislevel="3">
        <Row popup="false" name="T1">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="T2">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="T1?1 &gt; 128"/>
          </Rhs>
        </Row>
        <Row popup="false" name="T4">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_select_item.Segment_item.action T2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="T6">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="if T2 then T1 else 0"/>
          </Rhs>
        </Row>
        <Row popup="false" name="T5">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_select_item.Fill_item.action T6"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="11777" y="5" open="true" selected="false" sform="false" next="7" name="U" caption="Image / Draw">
      <Subcolumn vislevel="3">
        <Row popup="false" name="U1">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="U2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_draw_item.Line_item.action U1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="U3">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_draw_item.Rect_item.action U1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="U4">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_draw_item.Circle_item.action U1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="U6">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="3" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="sx">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Start x"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="100"/>
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
              <Row name="sy">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Start y"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="100"/>
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
              <Row name="e">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Flood while" labelsn="2" labels0="Not equal to ink" labels1="Equal to start point" value="1"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="i">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Ink"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="[255]"/>
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
            <iText formula="Image_draw_item.Flood_item.action U4"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="11129" y="5" open="true" selected="false" sform="false" next="11" name="V" caption="Image / Select">
      <Subcolumn vislevel="3">
        <Row popup="false" name="V1">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="V2">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="V1?1 &lt; 50"/>
          </Rhs>
        </Row>
        <Row popup="false" name="V3">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="V1?1 &lt; 100"/>
          </Rhs>
        </Row>
        <Row popup="false" name="V4">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="V1?1 &lt; 150"/>
          </Rhs>
        </Row>
        <Row popup="false" name="V5">
          <Rhs vislevel="1" flags="4">
            <iText formula="[V2, V3, V4]"/>
          </Rhs>
        </Row>
        <Row popup="false" name="V7">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false" scale="70.687610868639311" offset="0" page="0" falsecolour="true" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="switch V5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="V9">
          <Rhs vislevel="2" flags="4">
            <iText formula="[V1, 100, Vector [200, 100, 100]]"/>
          </Rhs>
        </Row>
        <Row popup="false" name="V10">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_select_item.Case_item.action V7 V9"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
  </Workspace>
</root>
