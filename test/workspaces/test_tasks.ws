<?xml version="1.0"?>
<root xmlns="http://www.vips.ecs.soton.ac.uk/nip/9.1.0">
  <Workspace window_x="6" window_y="56" window_width="1022" window_height="605" view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="" name="test_tasks" filename="$HOME/GIT/nip4/test/workspaces/test_tasks.ws" major="9" minor="1">
    <Column x="10" y="5" open="true" selected="false" sform="false" next="20" name="B" caption="Test images">
      <Subcolumn vislevel="3">
        <Row popup="false" name="B1">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="0" window_y="30" window_width="512" window_height="729"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/slanted_oval_vase2.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B17">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/print_test_image.v&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B18">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.sRGB_item.action B17"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1041" y="5" open="true" selected="false" sform="false" next="35" name="BC" caption="Tasks / Capture">
      <Subcolumn vislevel="3">
        <Row popup="false" name="BC2">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="0" window_y="30" window_width="512" window_height="729" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BC3">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1"/>
            <iText formula="Tasks_capture_item.Smooth_image_item.action BC2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BC4">
          <Rhs vislevel="2" flags="5">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_capture_item.Light_correct_item.action BC2 BC3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BC34">
          <Rhs vislevel="1" flags="1">
            <iRegion>
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region BC2 284 81 60 65"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BC13">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1"/>
            <iText formula="Tasks_capture_item.White_balance_item.action BC34 BC2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="2649" y="5" open="true" selected="false" sform="false" next="32" name="UB" caption="Tasks / Mosaic / Onepoint|Twopoint">
      <Subcolumn vislevel="3">
        <Row popup="false" name="UB1">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="5" window_y="54" window_width="512" window_height="729"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB2">
          <Rhs vislevel="1" flags="1">
            <iRegion window_x="193" window_y="129" window_width="613" window_height="909" left="27" top="54" width="202" height="453">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region UB1 50 171 202 228"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB3">
          <Rhs vislevel="1" flags="1">
            <iRegion window_x="848" window_y="305" window_width="613" window_height="284" left="180" top="75" width="180" height="448">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region UB1 193 248 180 243"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB4">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark UB2 182 53"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB5">
          <Rhs vislevel="1" flags="1">
            <iArrow left="30" top="31" width="0" height="0">
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark UB3 11 22"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB8">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_1point_item.Left_right_item.action UB4 UB5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB10">
          <Rhs vislevel="1" flags="1">
            <iRegion window_x="0" window_y="30" window_width="613" window_height="237">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region UB1 52 189 372 196"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB12">
          <Rhs vislevel="1" flags="1">
            <iRegion window_x="5" window_y="54" window_width="613" window_height="249">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region UB1 12 250 433 208"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB13">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark UB10 318 104"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB14">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark UB12 362 47"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB15">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_1point_item.Top_bottom_item.action UB13 UB14"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB16">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_1point_item.Left_right_manual_item.action UB4 UB5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB17">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_1point_item.Top_bottom_manual_item.action UB13 UB14"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB31">
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
                  <Slider caption="Angle" from="-180" to="180" value="-4"/>
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
            <iText formula="Tasks_capture_item.Rotate_item.Free_item.action UB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB19">
          <Rhs vislevel="1" flags="1">
            <iRegion window_x="463" window_y="317" window_width="446" window_height="260">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region UB31 26 289 430 219"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB20">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark UB19 378 40"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB21">
          <Rhs vislevel="1" flags="1">
            <iArrow left="139" top="42" width="0" height="0">
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark UB19 150 23"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB22">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark UB10 81 131"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB23">
          <Rhs vislevel="2" flags="5">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_2point_item.Top_bottom_item.action UB13 UB20 UB21 UB22"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB24">
          <Rhs vislevel="1" flags="1">
            <iRegion window_x="849" window_y="110" window_width="374" window_height="497">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region UB31 213 78 158 456"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB25">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark UB24 45 42"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB26">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark UB24 8 402"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB27">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark UB2 175 410"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB28">
          <Rhs vislevel="2" flags="5">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_2point_item.Left_right_item.action UB4 UB27 UB26 UB25"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="3331" y="5" open="true" selected="false" sform="false" next="38" name="IB" caption="Tasks / Mosaic / Manual Balance">
      <Subcolumn vislevel="3">
        <Row popup="false" name="IB8">
          <Rhs vislevel="1" flags="4">
            <iText formula="&quot;$VIPSHOME/share/nip4/data/examples/manual_balance/&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="IB1">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="602" window_y="480" window_width="745" window_height="689"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file (IB8 ++ &quot;simp_base.png&quot;)"/>
          </Rhs>
        </Row>
        <Row popup="false" name="IB2">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file (IB8 ++ &quot;mask_control.png&quot;)"/>
          </Rhs>
        </Row>
        <Row popup="false" name="IB3">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file (IB8 ++ &quot;mask_01.png&quot;)"/>
          </Rhs>
        </Row>
        <Row popup="false" name="IB4">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file (IB8 ++ &quot;mask_02.png&quot;)"/>
          </Rhs>
        </Row>
        <Row popup="false" name="IB5">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file (IB8 ++ &quot;mask_03.png&quot;)"/>
          </Rhs>
        </Row>
        <Row popup="false" name="IB6">
          <Rhs vislevel="2" flags="5">
            <Subcolumn vislevel="0"/>
            <iText formula="Group [IB3,IB4,IB5]"/>
            <Group/>
          </Rhs>
        </Row>
        <Row popup="false" name="IB9">
          <Rhs vislevel="1" flags="1">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Manual_balance_item.Balance_find_item.action IB1 IB2 IB6"/>
          </Rhs>
        </Row>
        <Row popup="false" name="IB10">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1">
              <Row name="im_in">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="m_matrix">
                <Rhs vislevel="1" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="m_group">
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
              <Row name="blur">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="adjust">
                <Rhs vislevel="1" flags="1">
                  <Matrix/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="Build">
                <Rhs vislevel="1" flags="1">
                  <Toggle caption="Build Scale and Offset Correction Images" value="true"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="Output">
                <Rhs vislevel="2" flags="6">
                  <Subcolumn vislevel="1"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="value">
                <Rhs vislevel="1" flags="4">
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Tasks_mosaic_item.Manual_balance_item.Balance_check_item.action IB1 IB9 IB6"/>
          </Rhs>
        </Row>
        <Row popup="false" name="IB11">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="2997" window_y="620" window_width="516" window_height="543"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Manual_balance_item.Balance_apply_item.action IB1 IB10.Output.scale_im IB10.Output.offset_im"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="4085" y="5" open="true" selected="false" sform="false" next="6" name="EC" caption="Tasks / Mosaic / Clone">
      <Subcolumn vislevel="3">
        <Row popup="false" name="EC4">
          <Rhs vislevel="1" flags="4">
            <iText formula="&quot;$VIPSHOME/share/nip4/data/examples/clone/&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="EC1">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="225" window_y="466" window_width="1108" window_height="532"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file (EC4 ++ &quot;example_im_01.png&quot;)"/>
          </Rhs>
        </Row>
        <Row popup="false" name="EC2">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="0" window_y="0" window_width="478" window_height="526"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file (EC4 ++ &quot;example_im_02.png&quot;)"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1552" y="5" open="true" selected="false" sform="false" next="8" name="C" caption="Tasks / Capture / Calib">
      <Subcolumn vislevel="3">
        <Row popup="false" name="C1">
          <Rhs vislevel="2" flags="5">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B18"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C2">
          <Rhs vislevel="1" flags="1">
            <iRegion left="6" top="0" width="543" height="367">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region C1 50 38 323 175"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C6">
          <Rhs vislevel="3" flags="7">
            <iImage/>
            <Subcolumn vislevel="1"/>
            <iText formula="Tasks_capture_item.Find_calib_item.action C2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C7">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_capture_item.Apply_calib_item.action C2 C6"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="411" y="5" open="true" selected="false" sform="false" next="3" name="A" caption="Tasks / Capture / CSV import">
      <Subcolumn vislevel="3">
        <Row popup="false" name="A2">
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
              <Row name="path">
                <Rhs vislevel="1" flags="1">
                  <Pathname caption="File to load" value="/home/john/GIT/nip4/test/workspaces/images/slanted_oval_vase2.csv"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="start_line">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Start at line"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="rows">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Lines to read (-1 for whole file)"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="whitespace">
                <Rhs vislevel="1" flags="1">
                  <String/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="separator">
                <Rhs vislevel="1" flags="1">
                  <String/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Tasks_capture_item.Csv_import_item.action"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="2208" y="5" open="true" selected="true" sform="false" next="5" name="D" caption="Tasks / Capture / Plot bands">
      <Subcolumn vislevel="3">
        <Row popup="false" name="D1">
          <Rhs vislevel="2" flags="5">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="copy [] B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D3">
          <Rhs vislevel="1" flags="1">
            <iRegion left="394" top="143" width="26" height="20">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region D1 433 32 26 20"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D4">
          <Rhs vislevel="3" flags="7">
            <Plot plot_left="0" plot_top="0" plot_mag="100" show_status="false"/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <Plot plot_left="0" plot_top="0" plot_mag="100" show_status="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="style">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="auto">
                <Rhs vislevel="1" flags="1">
                  <Toggle caption="Auto Range" value="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ymin">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Y range minimum"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ymax">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Y range maximum"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="255"/>
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
            <iText formula="Tasks_capture_item.Graph_bands_item.action D3"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
  </Workspace>
  <Workspace view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="// local definitions for this tab&#10;" name="reflectogram" filename="$HOME/GIT/nip4/test/workspaces/test_tasks.ws" major="9" minor="1">
    <Column x="10" y="5" open="true" selected="false" sform="false" next="12" name="A" caption="source images">
      <Subcolumn vislevel="3">
        <Row popup="false" name="A1">
          <Rhs vislevel="1" flags="1">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/cd1.1.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A2">
          <Rhs vislevel="1" flags="1">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/cd1.2.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A3">
          <Rhs vislevel="1" flags="1">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/cd2.1.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A4">
          <Rhs vislevel="1" flags="1">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/cd2.2.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A5">
          <Rhs vislevel="1" flags="1">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/cd3.1.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A6">
          <Rhs vislevel="1" flags="1">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/cd3.2.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A7">
          <Rhs vislevel="1" flags="1">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/cd4.1.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A8">
          <Rhs vislevel="1" flags="1">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/cd4.2.jpg&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="309" y="5" open="true" selected="false" sform="false" next="13" name="B" caption="left-right joins for all rows">
      <Subcolumn vislevel="3">
        <Row popup="false" name="B1">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A1 494 138"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B2">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A2 68 141"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B3">
          <Rhs vislevel="1" flags="1">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_1point_item.Left_right_item.action B1 B2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B4">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A3 460 52"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B5">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A4 23 53"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B6">
          <Rhs vislevel="1" flags="1">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_1point_item.Left_right_item.action B4 B5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B7">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A5 454 122"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B8">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A6 19 123"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B9">
          <Rhs vislevel="1" flags="1">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_1point_item.Left_right_item.action B7 B8"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B10">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A7 487 57"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B11">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A8 31 60"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B12">
          <Rhs vislevel="1" flags="1">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_1point_item.Left_right_item.action B10 B11"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="798" y="5" open="true" selected="false" sform="false" next="10" name="D" caption="top-bottom join of rows">
      <Subcolumn vislevel="3">
        <Row popup="false" name="D1">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark B3 362 346"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D2">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark B6 390 42"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D3">
          <Rhs vislevel="1" flags="1">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_1point_item.Top_bottom_item.action D1 D2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D4">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark D3 440 639"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D5">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark B9 441 31"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D6">
          <Rhs vislevel="1" flags="1">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_1point_item.Top_bottom_item.action D4 D5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D7">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark B12 441 23"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D8">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark D6 417 948"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D9">
          <Rhs vislevel="1" flags="1">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_1point_item.Top_bottom_item.action D7 D8"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1300" y="5" open="true" selected="false" sform="false" next="6" name="E" caption="balance mosaic">
      <Subcolumn vislevel="3">
        <Row popup="false" name="E1">
          <Rhs vislevel="2" flags="5">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="D9"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E3">
          <Rhs vislevel="2" flags="5">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Balance_item.action E1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E4">
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
              <Row name="tilt">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Top-bottom tilt" from="-1" to="1" value="0.26000000000000001"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Tasks_mosaic_item.Tilt_item.Top_bottom_item.action E3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E5">
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
              <Row name="tilt">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Left-right tilt" from="-1" to="1" value="0.21999999999999997"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Tasks_mosaic_item.Tilt_item.Left_right_item.action E4"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1802" y="5" open="true" selected="true" sform="false" next="3" name="C" caption="sharpen">
      <Subcolumn vislevel="3">
        <Row popup="false" name="C1">
          <Rhs vislevel="2" flags="5">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="E5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C2">
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
              <Row name="type">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Type" labelsn="2" labels0="Blur" labels1="Sharpen" value="1"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="r">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Radius" from="1" to="100" value="8"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="fac">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Amount" from="0" to="1" value="0.69999999999999996"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="layers">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="shape">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Mask shape" labelsn="2" labels0="Square" labels1="Gaussian" value="1"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="prec">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_conv_item.Custom_blur_item.action C1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
  </Workspace>
  <Workspace view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="// local definitions for this tab&#10;" name="xray" filename="$HOME/GIT/nip4/test/workspaces/test_tasks.ws" major="9" minor="1">
    <Column x="10" y="5" open="true" selected="false" sform="false" next="7" name="A" caption="source images">
      <Subcolumn vislevel="3">
        <Row popup="false" name="A1">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="49" window_y="31" window_width="454" window_height="528"/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/example_im_01.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A2">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="521" window_y="27" window_width="477" window_height="551"/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/example_im_02.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A3">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="10" window_y="22" window_width="464" window_height="539"/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/example_im_03.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A4">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="500" window_y="18" window_width="474" window_height="550"/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/example_im_04.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A5">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="18" window_y="6" window_width="473" window_height="547"/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/example_im_05.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A6">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="536" window_y="9" window_width="464" window_height="539"/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/example_im_06.jpg&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="364" y="5" open="true" selected="false" sform="false" next="27" name="B" caption="assemble mosaic">
      <Subcolumn vislevel="3">
        <Row popup="false" name="B1">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A1 417 225"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B2">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A2 102 220"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B3">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A2 53 480"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B4">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A1 375 481"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B5">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="0" window_y="6" window_width="750" window_height="552"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_2point_item.Left_right_item.action B1 B2 B3 B4"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B6">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A3 372 182"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B7">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A4 17 211"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B8">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A3 409 453"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B9">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A4 58 492"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B10">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="0" window_y="597" window_width="750" window_height="568"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_2point_item.Left_right_item.action B6 B7 B8 B9"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B11">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A5 401 38"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B12">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A6 59 60"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B13">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A5 385 398"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B14">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A6 32 416"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B15">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="2" window_y="779" window_width="750" window_height="575"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_2point_item.Left_right_item.action B11 B12 B13 B14"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B16">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark B10 111 44"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B17">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark B5 104 402"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B18">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark B10 655 37"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B19">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark B5 632 403"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B20">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="0" window_y="6" window_width="750" window_height="750"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_2point_item.Top_bottom_item.action B16 B17 B18 B19"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B21">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark B15 129 39"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B22">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark B20 129 824"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B23">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark B20 644 836"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B24">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark B15 655 42"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B25">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="0" window_y="6" window_width="750" window_height="750"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_2point_item.Top_bottom_item.action B21 B22 B23 B24"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="695" y="5" open="true" selected="true" sform="false" next="3" name="C" caption="balance mosaic">
      <Subcolumn vislevel="3">
        <Row popup="false" name="C1">
          <Rhs vislevel="0" flags="4">
            <iImage/>
            <Subcolumn vislevel="0"/>
            <iText formula="B25"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C2">
          <Rhs vislevel="2" flags="5">
            <iImage scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Balance_item.action C1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
  </Workspace>
  <Workspace view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="// local definitions for this tab&#10;" name="framing" filename="$HOME/GIT/nip4/test/workspaces/test_tasks.ws" major="9" minor="1">
    <Column x="10" y="5" open="true" selected="true" sform="false" next="5" name="A" caption="source images">
      <Subcolumn vislevel="3">
        <Row popup="false" name="A1">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/framing_picture.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A2">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/framing_complex.png&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A3">
          <Rhs vislevel="1" flags="1">
            <iImage/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/framing_corner.png&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A4">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="2172" window_y="197" window_width="664" window_height="802"/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/framing_distorted_frame.png&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="10" y="437" open="true" selected="false" sform="false" next="2" name="B" caption="Staighten Frame">
      <Subcolumn vislevel="3">
        <Row popup="false" name="B1">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="717" window_y="150" window_width="710" window_height="794"/>
            <Subcolumn vislevel="1">
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="dir">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap1">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="23" top="43" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap2">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="369" top="8" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap3">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="388" top="468" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ap4">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="9" top="485" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="x">
                <Rhs vislevel="0" flags="4">
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
            <iText formula="Tasks_frame_item.Straighten_frame_item.action A4"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="532" y="5" open="true" selected="false" sform="false" next="2" name="C" caption="Painting with Simple Frame">
      <Subcolumn vislevel="3">
        <Row popup="false" name="C1">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="1708" window_y="57" window_width="521" window_height="430"/>
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
              <Row name="ppcm">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Number of pixels per cm"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="5"/>
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
              <Row name="overlap">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Size of frame overlap in cm"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="variables">
                <Rhs vislevel="1" flags="4">
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="mount_options">
                <Rhs vislevel="1" flags="4">
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="frame">
                <Rhs vislevel="1" flags="1">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Tasks_frame_item.Build_frame_item.Simple_frame_item.action B1 A1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="532" y="433" open="true" selected="false" sform="false" next="2" name="D" caption="Painting with Complex Frame, with adjusted variables">
      <Subcolumn vislevel="3">
        <Row popup="false" name="D1">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="1673" window_y="48" window_width="548" window_height="445"/>
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
              <Row name="ppcm">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Number of pixels per cm"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="5"/>
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
              <Row name="overlap">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Size of frame overlap in cm"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="variables">
                <Rhs vislevel="2" flags="6">
                  <Subcolumn vislevel="1">
                    <Row name="super">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="scale_factor">
                      <Rhs vislevel="1" flags="1">
                        <Expression caption="scale the size of the frame by"/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="corner_section">
                      <Rhs vislevel="1" flags="1">
                        <Slider caption="Corner section" from="0.10000000000000001" to="1" value="0.5"/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="edge_section">
                      <Rhs vislevel="1" flags="1">
                        <Slider/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="middle_section">
                      <Rhs vislevel="1" flags="1">
                        <Slider/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="blend_fraction">
                      <Rhs vislevel="1" flags="1">
                        <Slider caption="Blend fraction" from="0.10000000000000001" to="0.90000000000000002" value="0.10000000000000001"/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="option">
                      <Rhs vislevel="1" flags="1">
                        <Toggle/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="comp">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="mount_options">
                <Rhs vislevel="1" flags="4">
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="frame">
                <Rhs vislevel="1" flags="1">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Tasks_frame_item.Build_frame_item.Complex_frame_item.action A2 A1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1283" y="5" open="true" selected="false" sform="false" next="2" name="E" caption="Painting with frame corner, and coloured mount">
      <Subcolumn vislevel="3">
        <Row popup="false" name="E1">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="1669" window_y="53" window_width="640" window_height="566"/>
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
              <Row name="ppcm">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Number of pixels per cm"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="5"/>
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
              <Row name="overlap">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Size of frame overlap in cm"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="-10"/>
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
              <Row name="variables">
                <Rhs vislevel="1" flags="4">
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="mount_options">
                <Rhs vislevel="2" flags="6">
                  <Subcolumn vislevel="1">
                    <Row name="super">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="apply">
                      <Rhs vislevel="1" flags="1">
                        <Toggle caption="Apply mount options" value="false"/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="ls">
                      <Rhs vislevel="1" flags="1">
                        <Expression caption="Lower mount section bigger by (cm)"/>
                        <Subcolumn vislevel="0">
                          <Row name="caption">
                            <Rhs vislevel="0" flags="4">
                              <iText/>
                            </Rhs>
                          </Row>
                          <Row name="expr">
                            <Rhs vislevel="0" flags="4">
                              <iText formula="5"/>
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
                    <Row name="mount_colour">
                      <Rhs vislevel="3" flags="7">
                        <Colour colour_space="sRGB" value0="0" value1="0" value2="0"/>
                        <Subcolumn vislevel="1"/>
                        <iText/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="frame">
                <Rhs vislevel="1" flags="1">
                  <iImage/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Tasks_frame_item.Build_frame_item.Frame_corner_item.action A3 A1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
  </Workspace>
</root>
