<?xml version="1.0"?>
<root xmlns="http://www.vips.ecs.soton.ac.uk/nip/9.1.0">
  <Workspace view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="// local definitions for this tab&#10;    mosaic_sort_test l&#10;        = error &quot;mosaic: not all points&quot;,&#10;            !is_listof is_Mark l&#10;        = error &quot;mosaic: points not on two images&quot;,&#10;            !is_list_len 2 images&#10;        = error &quot;mosaic: images do not match in format and coding&quot;,&#10;            !all_equal (map get_format images) || !all_equal (map get_coding images)&#10;        = error &quot;mosaic: not same number of points on each image&quot;,&#10;            !foldr1 equal (map len l')&#10;        = l'&#10;    {&#10;        // test for all elements of a list equal&#10;        all_equal l = all (map (equal (hd l)) (tl l));&#10;&#10;        // all the different images&#10;        images = mkset pointer_equal (map get_image l);&#10;&#10;        // find all points defined on image&#10;        test_image image p = (get_image p) === image;&#10;        find l image = filter (test_image image) l;&#10;&#10;        // group point list by image&#10;        l' = map (find l) images;&#10;    }&#10;" name="tab2" filename="$CWD/x.ws" major="9" minor="1">
    <Column x="10" y="5" open="true" selected="true" sform="false" next="11" name="A">
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
            <iRegion show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region A1 40 99 283 384"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A3">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage" left="300" top="200" width="196" height="340">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region A1 209 205 287 335"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A4">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A2 271 197"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A5">
          <Rhs vislevel="1" flags="1">
            <iArrow>
              <iRegiongroup/>
            </iArrow>
            <Subcolumn vislevel="0"/>
            <iText formula="Mark A3 9 99"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A7">
          <Rhs vislevel="1" flags="4">
            <iText formula="[A4, A5]"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A8">
          <Rhs vislevel="1" flags="4">
            <iText formula="mosaic_sort_test A7"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
  </Workspace>
</root>
