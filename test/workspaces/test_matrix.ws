<?xml version="1.0"?>
<root xmlns="http://www.vips.ecs.soton.ac.uk/nip/9.0.17">
  <Workspace window_x="6" window_y="56" window_width="1022" window_height="605" view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="// private definitions for this workspace&#10;" name="test_matrix" filename="$HOME/GIT/nip4/test/workspaces/test_matrix.ws" major="9" minor="1">
    <Column x="10" y="5" open="true" selected="false" sform="false" next="11" name="TB" caption="Matrix / New">
      <Subcolumn vislevel="3">
        <Row popup="false" name="TB1">
          <Rhs vislevel="2" flags="5">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Matrix_build_item.Plain_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="TB2">
          <Rhs vislevel="2" flags="5">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Matrix_build_item.Convolution_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="TB3">
          <Rhs vislevel="2" flags="5">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Matrix_build_item.Recombination_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="TB4">
          <Rhs vislevel="2" flags="5">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Matrix_build_item.Morphology_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="TB7">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix_build_item.Matrix_identity_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="TB8">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix_build_item.Matrix_series_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="TB9">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix_build_item.Matrix_square_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="TB10">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix_build_item.Matrix_circular_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="TB5">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix_build_item.Matrix_gaussian_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="TB6">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix_build_item.Matrix_laplacian_item.action"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1440" y="5" open="true" selected="false" sform="false" next="8" name="VB" caption="Matrix / Extract">
      <Subcolumn vislevel="3">
        <Row popup="false" name="VB1">
          <Rhs vislevel="1" flags="1">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="TB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="VB2">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix_extract_item.Rows_item.action VB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="VB6">
          <Rhs vislevel="1" flags="4">
            <iText formula="if VB2.value != [[1, 0, 0]] then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="VB3">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix_extract_item.Columns_item.action VB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="VB5">
          <Rhs vislevel="1" flags="4">
            <iText formula="if VB3.value != [[1], [0], [0]] then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="VB4">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix_extract_item.Diagonal_item.action VB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="VB7">
          <Rhs vislevel="1" flags="4">
            <iText formula="if VB4.value != [[1], [1], [1]] then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1860" y="5" open="true" selected="false" sform="false" next="7" name="WB" caption="Matrix / Insert">
      <Subcolumn vislevel="3">
        <Row popup="false" name="WB1">
          <Rhs vislevel="1" flags="1">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="TB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="WB2">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix_insert_item.Rows_item.action WB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="WB5">
          <Rhs vislevel="1" flags="4">
            <iText formula="if WB2.value?0 != [0, 0, 0] then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="WB3">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix_insert_item.Columns_item.action WB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="WB6">
          <Rhs vislevel="1" flags="4">
            <iText formula="if WB3.value?0 != [0, 1, 0, 0] then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="2373" y="5" open="true" selected="false" sform="false" next="7" name="XB" caption="Matrix / Delete">
      <Subcolumn vislevel="3">
        <Row popup="false" name="XB1">
          <Rhs vislevel="1" flags="1">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="TB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="XB2">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix_delete_item.Rows_item.action XB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="XB4">
          <Rhs vislevel="1" flags="4">
            <iText formula="if XB2.value?0 != [0, 1, 0] then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="XB3">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix_delete_item.Columns_item.action XB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="XB6">
          <Rhs vislevel="1" flags="4">
            <iText formula="if XB3.value?0 != [0, 0] then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="3502" y="5" open="true" selected="false" sform="false" next="6" name="YB" caption="Matrix / Rotate">
      <Subcolumn vislevel="3">
        <Row popup="false" name="YB1">
          <Rhs vislevel="1" flags="1">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="TB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="YB2">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix_rotate_item.rot90.action XB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="YB3">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix_rotate_item.rot180.action YB2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="YB4">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix_rotate_item.rot270.action YB3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="YB5">
          <Rhs vislevel="2" flags="5">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Matrix_rotate_item.Matrix_rot45_item.action YB4"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="3981" y="5" open="true" selected="false" sform="false" next="4" name="ZB" caption="Matrix / Flip">
      <Subcolumn vislevel="3">
        <Row popup="false" name="ZB1">
          <Rhs vislevel="1" flags="1">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="TB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="ZB2">
          <Rhs vislevel="1" flags="1">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Matrix_flip_item.Left_right_item.action ZB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="ZB3">
          <Rhs vislevel="1" flags="1">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Matrix_flip_item.Top_bottom_item.action ZB2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="4829" y="5" open="true" selected="false" sform="false" next="14" name="AC" caption="Matrix / Invert, Transpose">
      <Subcolumn vislevel="3">
        <Row popup="false" name="AC1">
          <Rhs vislevel="1" flags="1">
            <Matrix valuen="9" value0="1" value1="13" value2="42" value3="12" value4="1" value5="2" value6="1" value7="22" value8="1" width="3" height="3" scale="1" offset="0" filename="" display="0"/>
            <Subcolumn vislevel="0"/>
            <iText formula="TB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="AC2">
          <Rhs vislevel="1" flags="1">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Matrix_invert_item.action AC1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="AC3">
          <Rhs vislevel="1" flags="1">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Matrix_transpose_item.action AC2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="2787" y="5" open="true" selected="true" sform="false" next="7" name="A" caption="Matrix / Join">
      <Subcolumn vislevel="3">
        <Row popup="false" name="A1">
          <Rhs vislevel="0" flags="4">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="TB1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A2">
          <Rhs vislevel="0" flags="4">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="TB2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A3">
          <Rhs vislevel="2" flags="5">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Matrix_join.Left_right_item.action A1 A2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A6">
          <Rhs vislevel="1" flags="4">
            <iText formula="if A3.width != A1.width + A2.width then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A4">
          <Rhs vislevel="2" flags="5">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Matrix_join.Top_bottom_item.action A1 A2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A5">
          <Rhs vislevel="1" flags="4">
            <iText formula="if A4.height != A1.height + A2.height then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="5241" y="5" open="true" selected="false" sform="false" next="1" name="B" caption="Matrix / Plot, Build LUT">
      <Subcolumn vislevel="3">
        <Row popup="false" name="B7">
          <Rhs vislevel="1" flags="4">
            <iText formula="[1, 10..360]"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B8">
          <Rhs vislevel="1" flags="4">
            <iText formula="map sin B7"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B9">
          <Rhs vislevel="1" flags="1">
            <iText formula="Matrix (zip2 B7 B8)"/>
            <Matrix/>
            <Subcolumn vislevel="0"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B10">
          <Rhs vislevel="3" flags="7">
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix_plot_scatter_item.action B9"/>
            <Plot plot_left="0" plot_top="0" plot_mag="100" show_status="false"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B11">
          <Rhs vislevel="2" flags="5">
            <Subcolumn vislevel="0"/>
            <iText formula="Matrix_buildlut_item.action B9"/>
            <Plot plot_left="0" plot_top="0" plot_mag="100" show_status="false"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="4393" y="5" open="true" selected="false" sform="false" next="3" name="C" caption="Matrix / Sort">
      <Subcolumn vislevel="3">
        <Row popup="false" name="C1">
          <Rhs vislevel="1" flags="1">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Matrix [[1, 2, 3], [4, 5, 6]]"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C2">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <Matrix/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="o">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="i">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Sort on index"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="d">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Direction" labelsn="2" labels0="Ascending" labels1="Descending" value="1"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Matrix_sort_item.action C1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C3">
          <Rhs vislevel="3" flags="7">
            <Matrix/>
            <Subcolumn vislevel="1">
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <Matrix/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="o">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Orientation" labelsn="2" labels0="Sort by column" labels1="Sort by row" value="1"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="i">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Sort on index"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="d">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Direction" labelsn="2" labels0="Ascending" labels1="Descending" value="1"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Matrix_sort_item.action C1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1028" y="5" open="true" selected="false" sform="false" next="3" name="D" caption="Matrix / Convert">
      <Subcolumn vislevel="3">
        <Row popup="false" name="D4">
          <Rhs vislevel="3" flags="7">
            <Subcolumn vislevel="1"/>
            <iText formula="Matrix [[1, 2, 3], [4, 5, 6], [7, 8, 9]]"/>
            <Matrix/>
          </Rhs>
        </Row>
        <Row popup="false" name="D1">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_convert_to_image_item.action D4"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D5">
          <Rhs vislevel="2" flags="5">
            <Matrix/>
            <Subcolumn vislevel="0"/>
            <iText formula="Matrix_to_matrix_item.action D1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D2">
          <Rhs vislevel="1" flags="4">
            <iText formula="if D4.value != D5.value then error &quot;fail&quot; else &quot;ok!&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
  </Workspace>
</root>
