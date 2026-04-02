<?xml version="1.0"?>
<root xmlns="http://www.vips.ecs.soton.ac.uk/nip/9.1.0">
  <Workspace view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="// local definitions for this tab&#10;" name="tab2" filename="$CWD/x.ws" major="9" minor="1">
    <Column x="10" y="5" open="true" selected="false" sform="false" next="5" name="A">
      <Subcolumn vislevel="3">
        <Row popup="false" name="A1">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$CWD/images/print_test_image.v&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_convert_item.sRGB_item.action A1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A3">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false" left="0" top="0" width="549" height="365">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region A2 178 85 371 271"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="402" y="5" open="true" selected="false" sform="false" next="17" name="B">
      <Subcolumn vislevel="3">
        <Row popup="false" name="B1">
          <Rhs vislevel="1" flags="4">
            <iText formula="&quot;$VIPSHOME/share/$PACKAGE/data/macbeth_lab_d65.mat&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B2">
          <Rhs vislevel="1" flags="1">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Matrix_file B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B7">
          <Rhs vislevel="1" flags="4">
            <iText formula="&quot; &quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B8">
          <Rhs vislevel="1" flags="4">
            <iText formula="B2.value"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B10">
          <Rhs vislevel="1" flags="1">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Matrix B8"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B11">
          <Rhs vislevel="1" flags="4">
            <iText formula="vips_image_from_matrix B10"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B13">
          <Rhs vislevel="1" flags="4">
            <iText formula="bandfold [] B11"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B14">
          <Rhs vislevel="1" flags="4">
            <iText formula="set_header $interpretation $multiband B13"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B12">
          <Rhs vislevel="1" flags="4">
            <iText formula="vips_colourspace [$from =&gt; Interpretation.LAB] B14 Interpretation.XYZ"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B15">
          <Rhs vislevel="1" flags="4">
            <iText formula="bandunfold [] B12"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B16">
          <Rhs vislevel="1" flags="1">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="vips_matrix_from_image B15"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1014" y="5" open="true" selected="true" sform="false" next="3" name="C">
      <Subcolumn vislevel="3">
        <Row popup="false" name="C1">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="B2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C2">
          <Rhs vislevel="2" flags="5">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="colourspace [$from =&gt; Interpretation.LAB] Interpretation.XYZ C1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
  </Workspace>
</root>
