<?xml version="1.0"?>
<root xmlns="http://www.vips.ecs.soton.ac.uk/nip/9.0.17">
  <Workspace view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="// local definitions for this tab&#10;" name="tab2" filename="$HOME/GIT/nip4/test/workspaces/test_colour2.ws" major="9" minor="0">
    <Column x="10" y="5" open="true" selected="false" sform="false" next="5" name="A" caption="Colour / New">
      <Subcolumn vislevel="3">
        <Row popup="false" name="A1">
          <Rhs vislevel="3" flags="7">
            <Colour/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_new_item.Widget_colour_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A4">
          <Rhs vislevel="3" flags="7">
            <Colour/>
            <Subcolumn vislevel="1">
              <Row name="space">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="default_value">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <Colour/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="L">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Lightness" from="0" to="100" value="38"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ab_slice">
                <Rhs vislevel="1" flags="1">
                  <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="point">
                <Rhs vislevel="1" flags="1">
                  <iArrow left="107" top="161" width="0" height="0">
                    <iRegiongroup/>
                  </iArrow>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Colour_new_item.LAB_colour.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A3">
          <Rhs vislevel="3" flags="7">
            <Colour/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <Colour/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="T">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="CCT" from="1800" to="25000" value="4120"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Colour_new_item.CCT_colour.action"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="449" y="5" open="true" selected="false" sform="false" next="52" name="B" caption="Colour / Convert to colour">
      <Subcolumn vislevel="3">
        <Row popup="false" name="B1">
          <Rhs vislevel="1" flags="4">
            <iText formula="1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B4">
          <Rhs vislevel="1" flags="4">
            <iText formula="[1, 2, 3]"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B5">
          <Rhs vislevel="1" flags="4">
            <iText formula="[[1, 2, 3], [4, 5, 6]]"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B8">
          <Rhs vislevel="1" flags="1">
            <Real/>
            <Subcolumn vislevel="0"/>
            <iText formula="Real B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B9">
          <Rhs vislevel="1" flags="1">
            <Subcolumn vislevel="0"/>
            <iText formula="Vector B4"/>
            <Vector/>
          </Rhs>
        </Row>
        <Row popup="false" name="B10">
          <Rhs vislevel="1" flags="1">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Matrix B5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B7">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;../images/slanted_oval_vase2.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B6">
          <Rhs vislevel="2" flags="5">
            <Subcolumn vislevel="0"/>
            <iText formula="Group [B8, B1, B4, B5, B8, B9, B10, B7]"/>
            <Group/>
          </Rhs>
        </Row>
        <Row popup="false" name="B2">
          <Rhs vislevel="2" flags="5">
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_to_colour_item.action B6"/>
            <Group/>
          </Rhs>
        </Row>
        <Row popup="false" name="B44">
          <Rhs vislevel="1" flags="1">
            <Colour/>
            <Subcolumn vislevel="0"/>
            <iText formula="B2.value?0"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B45">
          <Rhs vislevel="1" flags="1">
            <Colour/>
            <Subcolumn vislevel="0"/>
            <iText formula="B2.value?1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B46">
          <Rhs vislevel="1" flags="1">
            <Colour/>
            <Subcolumn vislevel="0"/>
            <iText formula="B2.value?2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B47">
          <Rhs vislevel="1" flags="1">
            <Colour/>
            <Subcolumn vislevel="0"/>
            <iText formula="B2.value?3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B48">
          <Rhs vislevel="1" flags="1">
            <Colour/>
            <Subcolumn vislevel="0"/>
            <iText formula="B2.value?4"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B49">
          <Rhs vislevel="1" flags="1">
            <Colour/>
            <Subcolumn vislevel="0"/>
            <iText formula="B2.value?5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B50">
          <Rhs vislevel="1" flags="1">
            <Colour/>
            <Subcolumn vislevel="0"/>
            <iText formula="B2.value?6"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B51">
          <Rhs vislevel="1" flags="1">
            <Colour/>
            <Subcolumn vislevel="0"/>
            <iText formula="B2.value?7"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1107" y="5" open="true" selected="true" sform="false" next="17" name="C" caption="Colour / Colourspace">
      <Subcolumn vislevel="3">
        <Row popup="false" name="C1">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;../images/slanted_oval_vase2.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.Mono_item.action C1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C3">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.GREY16_item.action C2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C4">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.scRGB_item.action C1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C5">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.sRGB_item.action C4"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C6">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.RGB16_item.action C5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C7">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.HSV_item.action C6"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C8">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.XYZ_item.action C7"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C9">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.Yxy_item.action C8"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C10">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.Lab_item.action C9"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C11">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.OKLab_item.action C10"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C12">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.LabQ_item.action C11"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C13">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.LabS_item.action C12"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C14">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.LCh_item.action C13"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C15">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.OKLCh_item.action C14"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C16">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.UCS_item.action C15"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
  </Workspace>
</root>
