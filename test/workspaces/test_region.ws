<?xml version="1.0"?>
<root xmlns="http://www.vips.ecs.soton.ac.uk/nip/9.1.0">
  <Workspace view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="// local definitions for this tab&#10;" name="tab2" filename="$CWD/test_region.ws" major="9" minor="1">
    <Column x="10" y="5" open="true" selected="false" sform="false" next="7" name="A" caption="Region / New">
      <Subcolumn vislevel="3">
        <Row popup="false" name="A1">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$CWD/images/slanted_oval_vase2.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A2">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false" left="189" top="143" width="248" height="344">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region_new_item.Region_item.action A1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A3">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Region_new_item.Arrow_item.action A1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A4">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Region_new_item.Mark_item.action A1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A5">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Region_new_item.HGuide_item.action A1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A6">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Region_new_item.VGuide_item.action A1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="390" y="5" open="true" selected="false" sform="false" next="7" name="C" caption="Region / Union, Intersection">
      <Subcolumn vislevel="3">
        <Row popup="false" name="C1">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="copy [] A1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C2">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false" left="240" top="89" width="204" height="255">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region C1 59 77 204 255"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C3">
          <Rhs vislevel="2" flags="5">
            <iRegion show_status="false" show_convert="false" left="118" top="244" width="196" height="245">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region C1 210 269 196 245"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C5">
          <Rhs vislevel="2" flags="5">
            <iRegion show_status="false" show_convert="false">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region_union_item.action C2 C3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C6">
          <Rhs vislevel="2" flags="5">
            <iRegion show_status="false" show_convert="false">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region_intersection_item.action C2 C3"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="760" y="5" open="true" selected="false" sform="false" next="4" name="B" caption="Region / Margin">
      <Subcolumn vislevel="3">
        <Row popup="false" name="B1">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="copy [] A1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B2">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false" left="187" top="254" width="113" height="234">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region B1 220 215 113 234"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B3">
          <Rhs vislevel="3" flags="7">
            <iRegion show_status="false" show_convert="false">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iRegion show_status="false" show_convert="false">
                    <iRegiongroup/>
                  </iRegion>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="margin">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Margin" from="-100" to="100" value="100"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Region_margin_adjust_item.action B2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1176" y="5" open="true" selected="false" sform="false" next="4" name="D" caption="Region / Length">
      <Subcolumn vislevel="3">
        <Row popup="false" name="D1">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="copy [] A1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D2">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Arrow D1 293 360 (-164) (-182)"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D3">
          <Rhs vislevel="1" flags="4">
            <iText formula="Region_length_item.action D2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1509" y="5" open="true" selected="true" sform="false" next="13" name="E" caption="Region / Contains">
      <Subcolumn vislevel="3">
        <Row popup="false" name="E1">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="copy [] A1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E2">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region E1 174 144 135 162"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E6">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region E1 230 198 44 54"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E7">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false" left="280" top="288" width="70" height="109">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region E1 282 449 70 109"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E8">
          <Rhs vislevel="1" flags="4">
            <iText formula="Region_contains_item.action E2 E6"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E9">
          <Rhs vislevel="1" flags="4">
            <iText formula="Region_contains_item.action E2 E7"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E10">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark E1 251 226"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E11">
          <Rhs vislevel="1" flags="4">
            <iText formula="Region_contains_item.action E6 E10"/>
          </Rhs>
        </Row>
        <Row popup="false" name="E12">
          <Rhs vislevel="1" flags="4">
            <iText formula="Region_empty_item.action E10"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
  </Workspace>
</root>
