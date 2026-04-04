<?xml version="1.0"?>
<root xmlns="http://www.vips.ecs.soton.ac.uk/nip/9.1.0">
  <Workspace view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="// local definitions for this tab&#10;" name="tab2" filename="$CWD/overlay_blend-nip9.ws" major="9" minor="1">
    <Column x="10" y="5" open="true" selected="false" sform="false" next="5" name="A" caption="Images">
      <Subcolumn vislevel="3">
        <Row popup="false" name="A1">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$HOME/GIT/nip2/share/nip2/data/examples/overlays_and_blending/blend_example_ir.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A2">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$HOME/GIT/nip2/share/nip2/data/examples/overlays_and_blending/blend_example_vis.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A3">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$HOME/GIT/nip2/share/nip2/data/examples/overlays_and_blending/blend_example_xray.jpg&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="390" y="5" open="true" selected="false" sform="false" next="5" name="B" caption="Complete Transparency">
      <Subcolumn vislevel="3">
        <Row popup="false" name="B1">
          <Rhs vislevel="3" flags="7">
            <iText formula="Filter_blend_item.Scale_blend_item.action A2 A1"/>
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="390" y="231" open="true" selected="true" sform="false" next="3" name="C" caption="Blending Between two, then three Images">
      <Subcolumn vislevel="3">
        <Row popup="false" name="C1">
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
              <Row name="blend_position">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Blend position" from="0" to="1" value="0.109375"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="blend_width">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Blend width" from="0" to="1" value="0.20547945205479454"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="direction">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_blend_item.Line_blend_item.action A1 A2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C2">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="1600" window_y="652" window_width="472" window_height="511" show_status="false" show_convert="true"/>
            <Subcolumn vislevel="1">
              <Row name="a">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="b">
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
              <Row name="blend_position">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Blend position" from="0" to="1" value="0.625"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="blend_width">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="direction">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_blend_item.Line_blend_item.action C1 A3"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="902" y="5" open="true" selected="false" sform="false" next="2" name="D" caption="Overlay">
      <Subcolumn vislevel="3">
        <Row popup="false" name="D1">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="2554" window_y="136" window_width="619" window_height="590" show_status="false" show_convert="true"/>
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
              <Row name="colour">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Colour overlay as" labelsn="6" labels0="Green over Red" labels1="Blue over Red" labels2="Red over Green" labels3="Red over Blue" labels4="Blue over Green" labels5="Green over Blue" value="0"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_overlay_header_item.action A2 A1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
  </Workspace>
</root>
