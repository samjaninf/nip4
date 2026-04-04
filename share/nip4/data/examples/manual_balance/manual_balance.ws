<?xml version="1.0"?>
<root xmlns="http://www.vips.ecs.soton.ac.uk/nip/9.1.0">
  <Workspace view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="// local definitions for this tab&#10;" name="tab2" filename="$CWD/manual_balance-nip9.ws" major="9" minor="1">
    <Column x="10" y="5" open="true" selected="true" sform="false" next="8" name="A" caption="Images">
      <Subcolumn vislevel="3">
        <Row popup="false" name="A1">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="602" window_y="480" window_width="745" window_height="689" show_status="true" show_convert="true"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$HOME/GIT/nip2/share/nip2/data/examples/manual_balance/simp_base.png&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A2">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$HOME/GIT/nip2/share/nip2/data/examples/manual_balance/mask_control.png&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A3">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$HOME/GIT/nip2/share/nip2/data/examples/manual_balance/mask_01.png&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A4">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$HOME/GIT/nip2/share/nip2/data/examples/manual_balance/mask_02.png&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A5">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$HOME/GIT/nip2/share/nip2/data/examples/manual_balance/mask_03.png&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A6">
          <Rhs vislevel="1" flags="1">
            <Subcolumn vislevel="0"/>
            <iText formula="Group [A3,A4,A5]"/>
            <Group/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="755" y="5" open="true" selected="false" sform="false" next="4" name="B" caption="Process">
      <Subcolumn vislevel="3">
        <Row popup="false" name="B1">
          <Rhs vislevel="1" flags="1">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Manual_balance_item.Balance_find_item.action A1 A2 A6"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B2">
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
            <iText formula="Tasks_mosaic_item.Manual_balance_item.Balance_check_item.action A1 B1 A6"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B3">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="2997" window_y="620" window_width="516" window_height="543" show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Tasks_mosaic_item.Manual_balance_item.Balance_apply_item.action A1 B2.Output.scale_im B2.Output.offset_im"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
  </Workspace>
</root>
