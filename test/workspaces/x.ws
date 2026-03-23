<?xml version="1.0"?>
<root xmlns="http://www.vips.ecs.soton.ac.uk/nip/9.0.17">
  <Workspace view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="// local definitions for this tab&#10;" name="tab2" filename="$CWD/x.ws" major="9" minor="0">
    <Column x="10" y="5" open="true" selected="true" sform="false" next="9" name="A">
      <Subcolumn vislevel="3">
        <Row popup="false" name="A1">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$HOME/pics/nina.jpg&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A2">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="A1?0 &lt; 10"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A3">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="A1?0 &lt; 40"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A4">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="A1 ?0 &lt; 90"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A5">
          <Rhs vislevel="1" flags="4">
            <iText formula="[A2, A3, A4]"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A7">
          <Rhs vislevel="2" flags="5">
            <iText formula="switch A5"/>
            <iImage show_status="false" show_convert="false" scale="43.054200221564656" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
  </Workspace>
</root>
