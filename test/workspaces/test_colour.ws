<?xml version="1.0"?>
<root xmlns="http://www.vips.ecs.soton.ac.uk/nip/9.0.17">
  <Workspace view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="// local definitions for this tab&#10;" name="tab2" filename="$CWD/test_colour.ws" major="9" minor="1">
    <Column x="390" y="5" open="true" selected="false" sform="false" next="8" name="A" caption="Colour / New">
      <Subcolumn vislevel="3">
        <Row popup="false" name="A1">
          <Rhs vislevel="3" flags="7">
            <Colour/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_new_item.Widget_colour_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A6">
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
                  <Slider caption="Lightness" from="0" to="100" value="50"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="ab_slice">
                <Rhs vislevel="1" flags="1">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="point">
                <Rhs vislevel="1" flags="1">
                  <iArrow>
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
        <Row popup="false" name="A7">
          <Rhs vislevel="1" flags="4">
            <iText formula="if A1.value != A6.value then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="829" y="5" open="true" selected="false" sform="false" next="65" name="B" caption="Colour / Convert to colour">
      <Subcolumn vislevel="3">
        <Row popup="false" name="B64">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="O1"/>
          </Rhs>
        </Row>
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
        <Row popup="false" name="B6">
          <Rhs vislevel="2" flags="5">
            <Subcolumn vislevel="0"/>
            <iText formula="Group [B8, B1, B4, B5, B8, B9, B10, B64]"/>
            <Group/>
          </Rhs>
        </Row>
        <Row popup="false" name="B52">
          <Rhs vislevel="2" flags="5">
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_to_colour_item.action B6"/>
            <Group/>
          </Rhs>
        </Row>
        <Row popup="false" name="B53">
          <Rhs vislevel="1" flags="1">
            <Colour/>
            <Subcolumn vislevel="0"/>
            <iText formula="B52.value?0"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B54">
          <Rhs vislevel="1" flags="1">
            <Colour/>
            <Subcolumn vislevel="0"/>
            <iText formula="B52.value?1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B55">
          <Rhs vislevel="1" flags="1">
            <Colour/>
            <Subcolumn vislevel="0"/>
            <iText formula="B52.value?2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B56">
          <Rhs vislevel="1" flags="1">
            <Colour/>
            <Subcolumn vislevel="0"/>
            <iText formula="B52.value?3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B57">
          <Rhs vislevel="1" flags="1">
            <Colour/>
            <Subcolumn vislevel="0"/>
            <iText formula="B52.value?4"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B58">
          <Rhs vislevel="1" flags="1">
            <Colour/>
            <Subcolumn vislevel="0"/>
            <iText formula="B52.value?5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B59">
          <Rhs vislevel="1" flags="1">
            <Colour/>
            <Subcolumn vislevel="0"/>
            <iText formula="B52.value?6"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B60">
          <Rhs vislevel="1" flags="1">
            <Colour/>
            <Subcolumn vislevel="0"/>
            <iText formula="B52.value?7"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B62">
          <Rhs vislevel="2" flags="4">
            <iText formula="if mean (B60 - Vector [143, 136, 136]) &gt; 0.05 then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1487" y="5" open="true" selected="false" sform="false" next="21" name="C" caption="Colour / Colourspace">
      <Subcolumn vislevel="3">
        <Row popup="false" name="C1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="O1"/>
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
        <Row popup="false" name="C17">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.CMC_item.action C15"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C18">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.sRGB_item.action C17"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C19">
          <Rhs vislevel="2" flags="4">
            <iText formula="if max (abs (C18 - C1)) &gt; 5 then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1918" y="5" open="true" selected="false" sform="false" next="19" name="D" caption="Colour / Tag as">
      <Subcolumn vislevel="3">
        <Row popup="false" name="D1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="O1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_tag_item.Mono_item.action D1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D3">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_tag_item.GREY16_item.action D2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D4">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_tag_item.scRGB_item.action D1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D5">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_tag_item.sRGB_item.action D4"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D6">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_tag_item.RGB16_item.action D5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D7">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_tag_item.HSV_item.action D6"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D8">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_tag_item.XYZ_item.action D7"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D9">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_tag_item.Yxy_item.action D8"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D10">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_tag_item.Lab_item.action D9"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D11">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_tag_item.OKLab_item.action D10"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D12">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_tag_item.LabQ_item.action D11"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D13">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_tag_item.LabS_item.action D12"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D14">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_tag_item.LCh_item.action D13"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D15">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_tag_item.OKLCh_item.action D14"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D16">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_tag_item.CMC_item.action D15"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D17">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_tag_item.sRGB_item.action D16"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D18">
          <Rhs vislevel="1" flags="4">
            <iText formula="if max (abs (D17 - D1)) &gt; 0 then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="2349" y="5" open="true" selected="false" sform="false" next="37" name="E" caption="Colour / Temperature">
      <Subcolumn vislevel="3">
        <Row popup="false" name="E5">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="O1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E3">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_temperature_item.Whitepoint_item.action E5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E21">
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
              <Row name="from">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Whitepoint" labelsn="10" labels0="D93" labels1="D75" labels2="D65" labels3="D55" labels4="D50" labels5="CIE A (2850K)" labels6="CIE B (4874K)" labels7="CIE C (6774K)" labels8="CIE E (illuminant free)" labels9="D3250" value="4"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="to">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Whitepoint" labelsn="10" labels0="D93" labels1="D75" labels2="D65" labels3="D55" labels4="D50" labels5="CIE A (2850K)" labels6="CIE B (4874K)" labels7="CIE C (6774K)" labels8="CIE E (illuminant free)" labels9="D3250" value="2"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Colour_temperature_item.Whitepoint_item.action E3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E25">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.sRGB_item.action E21"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E24">
          <Rhs vislevel="1" flags="4">
            <iText formula="if max (abs (E5 - E25)) &gt; 0 then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E10">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_temperature_item.D65_to_D50_item.XYZ_minimal_item.action E5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E15">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_temperature_item.D50_to_D65_item.XYZ_minimal_item.action E10"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E12">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_temperature_item.D65_to_D50_item.Bradford_item.action E15"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E16">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_temperature_item.D50_to_D65_item.Bradford_item.action E12"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E1">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.sRGB_item.action E16"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E27">
          <Rhs vislevel="1" flags="4">
            <iText formula="if max (abs (E5 - E1)) &gt; 0 then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E28">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_temperature_item.Lab_to_D50XYZ_item.action E5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E29">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_temperature_item.D50XYZ_to_Lab_item.action E28"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E33">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.sRGB_item.action E29"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E32">
          <Rhs vislevel="1" flags="4">
            <iText formula="if max (abs (E5 - E33)) &gt; 0 then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E34">
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
                  <Slider caption="CCT" from="1800" to="25000" value="4000"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Colour_temperature_item.Colour_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E35">
          <Rhs vislevel="1" flags="4">
            <iText formula="Colour_temperature_item.CCT_item.action E34"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E36">
          <Rhs vislevel="1" flags="4">
            <iText formula="if max (abs (E35 - E34.T.value)) &gt; 10 then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="2959" y="5" open="true" selected="false" sform="false" next="4" name="F" caption="Colour / ICC">
      <Subcolumn vislevel="3">
        <Row popup="false" name="F1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="O1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="F7">
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
              <Row name="embedded">
                <Rhs vislevel="1" flags="1">
                  <Toggle caption="Use embedded profile if possible" value="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="profile">
                <Rhs vislevel="1" flags="1">
                  <Pathname/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="intent">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Intent" labelsn="5" labels0="Perceptual" labels1="Relative" labels2="Saturation" labels3="Absolute" labels4="Auto" value="3"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Colour_icc_item.Import_item.action F1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="F8">
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
              <Row name="profile">
                <Rhs vislevel="1" flags="1">
                  <Pathname caption="Output profile" value="/home/john/vips/share/nip4/data/sRGB.icm"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="intent">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="depth">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Colour_icc_item.Export_item.action F7"/>
          </Rhs>
        </Row>
        <Row popup="false" name="F2">
          <Rhs vislevel="1" flags="4">
            <iText formula="if max (abs (F1 - F8)) &gt; 0 then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="F9">
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
              <Row name="in_profile">
                <Rhs vislevel="1" flags="1">
                  <Pathname/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="out_profile">
                <Rhs vislevel="1" flags="1">
                  <Pathname caption="Output profile" value="/home/john/vips/share/nip4/data/sRGB.icm"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="intent">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Colour_icc_item.Transform_item.action F1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="F3">
          <Rhs vislevel="1" flags="4">
            <iText formula="if max (abs (F1 - F9)) &gt; 0 then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="3433" y="5" open="true" selected="false" sform="false" next="9" name="G" caption="Colour / Radiance">
      <Subcolumn vislevel="3">
        <Row popup="false" name="G2">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false" scale="0.062889984922387107" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="O2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="G3">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_rad_item.Unpack_item.action G2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="G4">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_rad_item.Pack_item.action G3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="G7">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_rad_item.Unpack_item.action G4"/>
          </Rhs>
        </Row>
        <Row popup="false" name="G8">
          <Rhs vislevel="1" flags="4">
            <iText formula="if  max (abs (G7 - G3)) &gt; 0 then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="4395" y="5" open="true" selected="false" sform="false" next="13" name="H" caption="Colour / Difference">
      <Subcolumn vislevel="3">
        <Row popup="false" name="H1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="O1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="H3">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="H1 + Vector [0, 10, 0]"/>
          </Rhs>
        </Row>
        <Row popup="false" name="H4">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false" scale="15.322951526408438" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_dE_item.CIEdE76_item.action H1 H3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="H7">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false" scale="9.2401992845974217" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_dE_item.CIEdE00_item.action H1 H3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="H6">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false" scale="7.3288117086398508" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_dE_item.CMC_item.action H1 H3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="H9">
          <Rhs vislevel="1" flags="4">
            <iText formula="if abs (mean [mean H4, mean H7, mean H6] - 8) &gt; 1 then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="4801" y="5" open="true" selected="false" sform="false" next="7" name="I" caption="Colour / Adjust">
      <Subcolumn vislevel="3">
        <Row popup="false" name="I2">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="O1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="I3">
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
              <Row name="matrix">
                <Rhs vislevel="1" flags="1">
                  <Matrix valuen="9" value0="1.0116470588235078" value1="0" value2="0" value3="0" value4="1.0625" value5="0" value6="0" value7="0" value8="-0.25" width="3" height="3" scale="1" offset="0" filename="" display="1"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Colour_adjust_item.Recombination_item.action I2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="I4">
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
              <Row name="gr">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Green-red" from="-20" to="20" value="-7.4285714285714288"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="by">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Blue-yellow" from="-20" to="20" value="11.857142857142854"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Colour_adjust_item.Cast_item.action I2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="I5">
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
              <Row name="h">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Hue" from="0" to="360" value="151.77700348432055"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="s">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Saturation" from="0.01" to="5" value="3.8524738675958186"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="b">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Brightness" from="0.01" to="5" value="0.91411149825783977"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Colour_adjust_item.HSB_item.action I2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="5459" y="5" open="true" selected="false" sform="false" next="4" name="J" caption="Colour / Similar">
      <Subcolumn vislevel="3">
        <Row popup="false" name="J1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="O1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="J3">
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
              <Row name="target_colour">
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
                        <Slider/>
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
                        <iArrow left="258" top="254" width="0" height="0">
                          <iRegiongroup/>
                        </iArrow>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="t">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Colour_similar_item.action J1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="6027" y="5" open="true" selected="false" sform="false" next="5" name="K" caption="Colour / Measure colour chart">
      <Subcolumn vislevel="3">
        <Row popup="false" name="K1">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="O3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="K2">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage" left="0" top="0" width="549" height="369">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region K1 131 116 223 184"/>
          </Rhs>
        </Row>
        <Row popup="false" name="K4">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_chart_to_matrix_item.action K2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="6540" y="5" open="true" selected="false" sform="false" next="8" name="L" caption="Colour / Make synthetic colour chart">
      <Subcolumn vislevel="3">
        <Row popup="false" name="L1">
          <Rhs vislevel="1" flags="1">
            <iText formula="K4"/>
            <Matrix/>
            <Subcolumn vislevel="0"/>
          </Rhs>
        </Row>
        <Row popup="false" name="L3">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_matrix_to_chart_item.action L1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="L4">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_tag_item.Lab_item.action L3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="L5">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_chart_to_matrix_item.action L4"/>
          </Rhs>
        </Row>
        <Row popup="false" name="L7">
          <Rhs vislevel="1" flags="4">
            <iText formula="if max (abs (L5 - K4)) &gt; 1 then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="7053" y="5" open="true" selected="false" sform="false" next="3" name="M" caption="Colour / Plot ab scatter">
      <Subcolumn vislevel="3">
        <Row popup="false" name="M1">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="O3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M2">
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
              <Row name="bins">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Number of bins on each axis"/>
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
            </Subcolumn>
            <iText formula="Colour_plot_ab_scatter_item.action M1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="3814" y="5" open="true" selected="false" sform="false" next="24" name="N" caption="Colour / UHDR">
      <Subcolumn vislevel="3">
        <Row popup="false" name="N1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="O4"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N23">
          <Rhs vislevel="1" flags="4">
            <iText formula="&quot; &quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N2">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_uhdr_item.Apply_gainmap_item.action N1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N3">
          <Rhs vislevel="1" flags="4">
            <iText formula="if  max N2 &lt; 1 then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N4">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_uhdr_item.Get_gainmap_item.action N1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N5">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="(unsigned char) (N4 + 10)"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N11">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_uhdr_item.Set_gainmap_item.action N5 N1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N13">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_uhdr_item.Apply_gainmap_item.action N11"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N14">
          <Rhs vislevel="1" flags="4">
            <iText formula="if max N13 &lt; max N2 then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N15">
          <Rhs vislevel="2" flags="6">
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="mincb">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Max content boost"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="maxcb">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Min content boost"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="gamma">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Gamma"/>
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
              <Row name="offsetsdr">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Offset SDR"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="offsethdr">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Offset HDR"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="hdrcapmin">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="HDR capacity min"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="hdrcapmax">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="HDR capacity max"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="basecg">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Use base cg"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="sf">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Scale factor"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Colour_uhdr_item.Get_uhdr_metadata_item.action N1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N16">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_uhdr_item.Set_uhdr_metadata_item.action N15 N1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N17">
          <Rhs vislevel="2" flags="6">
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_uhdr_item.Get_uhdr_metadata_item.action N16"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N20">
          <Rhs vislevel="1" flags="4">
            <iText formula="if N17.gamma.expr != 12 then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="10" y="5" open="true" selected="true" sform="false" next="5" name="O" caption="Test images">
      <Subcolumn vislevel="3">
        <Row popup="false" name="O1">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/slanted_oval_vase2.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="O2">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false" scale="0.016860462165912533" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/sample.hdr&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="O3">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/print_test_image.v&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="O4">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
	    <iText formula="Image_file &quot;$HOME/GIT/nip4/test/workspaces/images/ultra-hdr.jpg&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
  </Workspace>
</root>
