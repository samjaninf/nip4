<?xml version="1.0"?>
<root xmlns="http://www.vips.ecs.soton.ac.uk/nip/9.1.0">
  <Workspace window_x="6" window_y="56" window_width="1022" window_height="605" view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="" name="test_tasks" filename="$CWD/test_tasks.ws" major="9" minor="1">
    <Column x="10" y="5" open="true" selected="false" sform="false" next="20" name="B" caption="Test images">
      <Subcolumn vislevel="3">
        <Row popup="false" name="B1">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="0" window_y="30" window_width="512" window_height="729" show_status="true" show_convert="true"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$VIPSHOME/share/nip4/data/examples/businesscard/slanted_oval_vase2.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B17">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$CWD/images/print_test_image.v&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B18">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.sRGB_item.action B17"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1040" y="5" open="true" selected="false" sform="false" next="35" name="BC" caption="Tasks / Capture">
      <Subcolumn vislevel="3">
        <Row popup="false" name="BC2">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="0" window_y="30" window_width="512" window_height="729" show_status="true" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BC3">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Tasks_capture_item.Smooth_image_item.action BC2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BC4">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_capture_item.Light_correct_item.action BC2 BC3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BC34">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region BC2 284 81 60 65"/>
          </Rhs>
        </Row>
        <Row popup="false" name="BC13">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Tasks_capture_item.White_balance_item.action BC34 BC2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="2642" y="5" open="true" selected="false" sform="false" next="32" name="UB" caption="Tasks / Mosaic / Onepoint|Twopoint">
      <Subcolumn vislevel="3">
        <Row popup="false" name="UB1">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="5" window_y="54" window_width="512" window_height="729" show_status="true" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB2">
          <Rhs vislevel="1" flags="1">
            <iRegion window_x="193" window_y="129" window_width="613" window_height="909" show_status="true" show_convert="false" left="27" top="54" width="202" height="453">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region UB1 50 171 202 228"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB3">
          <Rhs vislevel="1" flags="1">
            <iRegion window_x="848" window_y="305" window_width="613" window_height="284" show_status="true" show_convert="false" left="180" top="75" width="180" height="448">
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
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_1point_item.Left_right_item.action UB4 UB5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB10">
          <Rhs vislevel="1" flags="1">
            <iRegion window_x="0" window_y="30" window_width="613" window_height="237" show_status="true" show_convert="false">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region UB1 52 189 372 196"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB12">
          <Rhs vislevel="1" flags="1">
            <iRegion window_x="5" window_y="54" window_width="613" window_height="249" show_status="true" show_convert="false">
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
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_1point_item.Top_bottom_item.action UB13 UB14"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB16">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_1point_item.Left_right_manual_item.action UB4 UB5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB17">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_1point_item.Top_bottom_manual_item.action UB13 UB14"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB31">
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
            <iRegion window_x="463" window_y="317" window_width="446" window_height="260" show_status="true" show_convert="false">
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
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_2point_item.Top_bottom_item.action UB13 UB20 UB21 UB22"/>
          </Rhs>
        </Row>
        <Row popup="false" name="UB24">
          <Rhs vislevel="1" flags="1">
            <iRegion window_x="849" window_y="110" window_width="374" window_height="497" show_status="true" show_convert="false">
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
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Mosaic_2point_item.Left_right_item.action UB4 UB27 UB26 UB25"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="3324" y="5" open="true" selected="false" sform="false" next="38" name="IB" caption="Tasks / Mosaic / Manual Balance">
      <Subcolumn vislevel="3">
        <Row popup="false" name="IB8">
          <Rhs vislevel="1" flags="4">
            <iText formula="&quot;$VIPSHOME/share/nip4/data/examples/manual_balance/&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="IB1">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="602" window_y="480" window_width="745" window_height="689" show_status="true" show_convert="true"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file (IB8 ++ &quot;simp_base.png&quot;)"/>
          </Rhs>
        </Row>
        <Row popup="false" name="IB2">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file (IB8 ++ &quot;mask_control.png&quot;)"/>
          </Rhs>
        </Row>
        <Row popup="false" name="IB3">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file (IB8 ++ &quot;mask_01.png&quot;)"/>
          </Rhs>
        </Row>
        <Row popup="false" name="IB4">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file (IB8 ++ &quot;mask_02.png&quot;)"/>
          </Rhs>
        </Row>
        <Row popup="false" name="IB5">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
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
            <iImage show_status="false" show_convert="false"/>
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
                  <iImage show_status="false" show_convert="false"/>
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
            <iImage window_x="2997" window_y="620" window_width="516" window_height="543" show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Manual_balance_item.Balance_apply_item.action IB1 IB10.Output.scale_im IB10.Output.offset_im"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="4075" y="5" open="true" selected="false" sform="false" next="6" name="EC" caption="Tasks / Mosaic / Clone">
      <Subcolumn vislevel="3">
        <Row popup="false" name="EC4">
          <Rhs vislevel="1" flags="4">
            <iText formula="&quot;$VIPSHOME/share/nip4/data/examples/clone/&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="EC1">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="225" window_y="466" window_width="1108" window_height="532" show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file (EC4 ++ &quot;example_im_01.png&quot;)"/>
          </Rhs>
        </Row>
        <Row popup="false" name="EC2">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="0" window_y="0" window_width="478" window_height="526" show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file (EC4 ++ &quot;example_im_02.png&quot;)"/>
          </Rhs>
        </Row>
        <Row popup="false" name="EC5">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="0" window_y="0" window_width="639" window_height="532" show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="im1">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="im2">
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
              <Row name="r1">
                <Rhs vislevel="1" flags="1">
                  <iRegion show_status="false" show_convert="false" left="406" top="49" width="28" height="37">
                    <iRegiongroup/>
                  </iRegion>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="p2">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="86" top="23" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="mask">
                <Rhs vislevel="1" flags="1">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="Options">
                <Rhs vislevel="3" flags="6">
                  <Subcolumn vislevel="2">
                    <Row name="super">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="pause">
                      <Rhs vislevel="1" flags="1">
                        <Toggle caption="Pause process" value="true"/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="replace">
                      <Rhs vislevel="1" flags="1">
                        <Option/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="balance">
                      <Rhs vislevel="1" flags="1">
                        <Toggle/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="process">
                      <Rhs vislevel="1" flags="1">
                        <Toggle caption="Replace area with Gaussian noise." value="false"/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="sc">
                      <Rhs vislevel="1" flags="1">
                        <Slider/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Tasks_mosaic_item.Clone_area_item.action EC1 EC2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1549" y="5" open="true" selected="false" sform="false" next="8" name="C" caption="Tasks / Capture / Calib">
      <Subcolumn vislevel="3">
        <Row popup="false" name="C1">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B18"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C2">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false" left="6" top="0" width="543" height="367">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region C1 50 38 323 175"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C6">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Tasks_capture_item.Find_calib_item.action C2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C7">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_capture_item.Apply_calib_item.action C2 C6"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="410" y="5" open="true" selected="false" sform="false" next="3" name="A" caption="Tasks / Capture / CSV import">
      <Subcolumn vislevel="3">
        <Row popup="false" name="A2">
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
    <Column x="2206" y="5" open="true" selected="true" sform="false" next="5" name="D" caption="Tasks / Capture / Plot bands">
      <Subcolumn vislevel="3">
        <Row popup="false" name="D1">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="copy [] B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D3">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false" left="394" top="143" width="26" height="20">
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
</root>
