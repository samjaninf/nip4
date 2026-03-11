<?xml version="1.0"?>
<root xmlns="http://www.vips.ecs.soton.ac.uk/nip/9.0.17">
  <Workspace window_x="6" window_y="56" window_width="1022" window_height="605" view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="// private definitions for this workspace&#10;" name="test_filter" filename="$HOME/GIT/nip4/test/workspaces/test_filter.ws" major="9" minor="0">
    <Column x="10" y="5" open="true" selected="false" sform="false" next="23" name="B" caption="Colour / Colourspace">
      <Subcolumn vislevel="3">
        <Row popup="false" name="B1">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="0" window_y="30" window_width="512" window_height="729" show_status="true" show_convert="true"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_file &quot;$VIPSHOME/share/nip4/data/examples/businesscard/slanted_oval_vase2.jpg&quot;"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="384" y="5" open="true" selected="false" sform="false" next="19" name="H" caption="Filter / Convolution">
      <Subcolumn vislevel="3">
        <Row popup="false" name="H1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="H9">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_conv_item.Blur_item.action H1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="H10">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_conv_item.Sharpen_item.action H1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="H15">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="0" window_y="30" window_width="512" window_height="729" show_status="true" show_convert="false"/>
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
              <Row name="size">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Radius" labelsn="6" labels0="3 pixels" labels1="5 pixels" labels2="7 pixels" labels3="9 pixels" labels4="11 pixels" labels5="51 pixels" value="4"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="st">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="bm">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="dm">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="fs">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="js">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_conv_item.Usharp_item.action H1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="H11">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_conv_item.Emboss_item.action H1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="H12">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="0" window_y="30" window_width="512" window_height="729" show_status="true" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_conv_item.Laplacian_item.action H1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="H13">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_conv_item.Sobel_item.action H1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="H14">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false" scale="7.3807648201667826" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_conv_item.Linedet_item.action H1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="H18">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="255.00000000000043" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_conv_item.Canny_item.action H9"/>
          </Rhs>
        </Row>
        <Row popup="false" name="H16">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="0" window_y="30" window_width="512" window_height="729" show_status="true" show_convert="false"/>
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
              <Row name="r">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Radius" from="1" to="100" value="27"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="shape">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Mask shape" labelsn="2" labels0="Square" labels1="Gaussian" value="1"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="type">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="fac">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="prec">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="layers">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_conv_item.Custom_blur_item.action H1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="H17">
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
              <Row name="matrix">
                <Rhs vislevel="1" flags="1">
                  <Matrix valuen="9" value0="-1" value1="-1" value2="-1" value3="-1" value4="9" value5="-1" value6="-1" value7="-1" value8="-1" width="3" height="3" scale="1" offset="0" filename="" display="3"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="separable">
                <Rhs vislevel="1" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="prec">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="rotate">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Rotate" labelsn="4" labels0="Don't rotate" labels1="4 x 45 degrees" labels2="8 x 45 degrees" labels3="2 x 90 degrees" value="0"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_conv_item.Custom_conv_item.action H1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1021" y="5" open="true" selected="false" sform="false" next="7" name="K" caption="Filter / Rank">
      <Subcolumn vislevel="3">
        <Row popup="false" name="K1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="K2">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_rank_item.Median_item.action K1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="K6">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_rank_item.Median_item.action K2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="K3">
          <Rhs vislevel="1" flags="1">
            <Subcolumn vislevel="0"/>
            <iText formula="Group [K1,K2]"/>
            <Group/>
          </Rhs>
        </Row>
        <Row popup="false" name="K4">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_rank_item.Image_rank_item.action K3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="K5">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_rank_item.Custom_rank_item.action K1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1751" y="5" open="true" selected="false" sform="false" next="12" name="M" caption="Filter / Morphology">
      <Subcolumn vislevel="3">
        <Row popup="false" name="M1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Image_select_item.Threshold_item.action M1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M3">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_morphology_item.Dilate_item.Dilate8_item.action M2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M4">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_morphology_item.Dilate_item.Dilate4_item.action M2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M5">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_morphology_item.Erode_item.Erode8_item.action M2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M6">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_morphology_item.Erode_item.Erode4_item.action M2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M7">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_morphology_item.Custom_morph_item.action M2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M8">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_morphology_item.Open_item.action M2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M9">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_morphology_item.Close_item.action M2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M10">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_morphology_item.Clean_item.action M2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="M11">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_morphology_item.Thin_item.action M2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="2260" y="5" open="true" selected="false" sform="false" next="16" name="N" caption="Filter / Fourier / Ideal">
      <Subcolumn vislevel="3">
        <Row popup="false" name="N1">
          <Rhs vislevel="0" flags="4">
            <iImage window_x="0" window_y="30" window_width="512" window_height="729" show_status="true" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N2">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false" left="187" top="158" width="256" height="256">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region N1 74 139 264 242"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N6">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_fourier_item.New_ideal_item.High_low_item.action N2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N13">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_fourier_item.New_ideal_item.Ring_item.action N2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="N14">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_fourier_item.New_ideal_item.Band_item.action N2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="2858" y="5" open="true" selected="false" sform="false" next="12" name="O" caption="Filter / Fourier / Gaussian">
      <Subcolumn vislevel="3">
        <Row popup="false" name="O1">
          <Rhs vislevel="0" flags="4">
            <iImage window_x="0" window_y="30" window_width="512" window_height="729" show_status="true" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="O2">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false" left="113" top="79" width="256" height="256">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region O1 74 139 264 242"/>
          </Rhs>
        </Row>
        <Row popup="false" name="O9">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_fourier_item.New_gaussian_item.High_low_item.action O2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="O10">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_fourier_item.New_gaussian_item.Ring_item.action O2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="O11">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_fourier_item.New_gaussian_item.Band_item.action O2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="3456" y="5" open="true" selected="false" sform="false" next="13" name="P" caption="Filter / Fourier / Butterworth">
      <Subcolumn vislevel="3">
        <Row popup="false" name="P1">
          <Rhs vislevel="0" flags="4">
            <iImage window_x="0" window_y="30" window_width="512" window_height="729" show_status="true" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="P2">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false" left="113" top="79" width="256" height="256">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region P1 74 139 264 242"/>
          </Rhs>
        </Row>
        <Row popup="false" name="P9">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_fourier_item.New_butterworth_item.High_low_item.action P2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="P10">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_fourier_item.New_butterworth_item.Ring_item.action P2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="P11">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_fourier_item.New_butterworth_item.Band_item.action P2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="4054" y="5" open="true" selected="false" sform="false" next="7" name="Q" caption="Filter / Enhance">
      <Subcolumn vislevel="3">
        <Row popup="false" name="Q1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="Q2">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Q1?1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="Q3">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_enhance_item.Falsecolour_item.action Q2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="Q4">
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
              <Row name="wsize">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Window size"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="tmean">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Target mean"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="200"/>
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
              <Row name="tdev">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Target deviation"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="100"/>
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
              <Row name="tmean_weight">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Mean weight" from="0" to="1" value="0.22033898305084745"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="tdev_weight">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Deviation weight" from="0" to="1" value="0.4576271186440678"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_enhance_item.Statistical_diff_item.action Q1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="Q5">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_enhance_item.Hist_equal_item.Global_item.action Q1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="Q6">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_enhance_item.Hist_equal_item.Local_item.action Q1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="5520" y="5" open="true" selected="false" sform="false" next="7" name="R" caption="Filter / Tilt Brightness">
      <Subcolumn vislevel="3">
        <Row popup="false" name="R1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="R2">
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
              <Row name="tilt">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Left-right tilt" from="-1" to="1" value="-0.60000000000000009"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_tilt_item.Left_right_item.action R1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="R3">
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
              <Row name="tilt">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Top-bottom tilt" from="-1" to="1" value="0.39999999999999991"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_tilt_item.Top_bottom_item.action R1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="R4">
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
              <Row name="tilt">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Left-right tilt" from="-1" to="1" value="-0.60000000000000009"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="shift">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Shift by" from="-1" to="1" value="-0.56989247311827951"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_tilt_item.Left_right_cos_item.action R1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="R5">
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
              <Row name="tilt">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Top-bottom tilt" from="-1" to="1" value="-1"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="shift">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Shift by" from="-1" to="1" value="0.60000000000000009"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_tilt_item.Top_bottom_cos_item.action R1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="R6">
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
              <Row name="tilt">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Tilt" from="-1" to="1" value="1"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="hshift">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Horizontal shift by" from="-1" to="1" value="-0.038644067796610115"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="vshift">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Vertical shift by" from="-1" to="1" value="0.0057627118644070219"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_tilt_item.Circular_item.action R1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="5999" y="5" open="true" selected="false" sform="false" next="15" name="S" caption="Filter / Blend">
      <Subcolumn vislevel="3">
        <Row popup="false" name="S1">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="388" window_y="128" window_width="547" window_height="727" show_status="true" show_convert="true" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="S2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Q3"/>
          </Rhs>
        </Row>
        <Row popup="false" name="S3">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
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
              <Row name="p">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Blend position" from="0" to="1" value="0.79999999999999993"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_blend_item.Scale_blend_item.action S1 S2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="S4">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="S3?1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="S9">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.U8_item.action S4"/>
          </Rhs>
        </Row>
        <Row popup="false" name="S5">
          <Rhs vislevel="3" flags="7">
            <Colour colour_space="sRGB" value0="13" value1="240" value2="65"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_new_item.Widget_colour_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="S6">
          <Rhs vislevel="3" flags="7">
            <Colour colour_space="sRGB" value0="231" value1="21" value2="13"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_new_item.Widget_colour_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="S10">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="0" window_y="30" window_width="512" window_height="729" show_status="true" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_blend_item.Image_blend_item.action S9 S5 S6"/>
          </Rhs>
        </Row>
        <Row popup="false" name="S11">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
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
              <Row name="orientation">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="blend_position">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Blend position" from="0" to="1" value="0.546875"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="blend_width">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Blend width" from="0" to="1" value="0.34999999999999998"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_blend_item.Line_blend_item.action S1 S2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="6511" y="5" open="true" selected="false" sform="false" next="5" name="T" caption="Filter / Overlay">
      <Subcolumn vislevel="3">
        <Row popup="false" name="T1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="T2">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="T1?0"/>
          </Rhs>
        </Row>
        <Row popup="false" name="T3">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="T1?1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="T4">
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
              <Row name="colour">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Colour overlay as" labelsn="6" labels0="Green over Red" labels1="Blue over Red" labels2="Red over Green" labels3="Red over Blue" labels4="Blue over Green" labels5="Green over Blue" value="3"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_overlay_header_item.action T2 T3"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="6967" y="5" open="true" selected="false" sform="false" next="7" name="U" caption="Filter / Colorize">
      <Subcolumn vislevel="3">
        <Row popup="false" name="U1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="U2">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="U1?0"/>
          </Rhs>
        </Row>
        <Row popup="false" name="U5">
          <Rhs vislevel="3" flags="7">
            <Colour colour_space="LAB" value0="71.953727722167969" value1="12.948423385620117" value2="74.347236633300781"/>
            <Subcolumn vislevel="1">
              <Row name="default_colour">
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
              <Row name="interp">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Interpretation" labelsn="10" labels0="sRGB" labels1="scRGB" labels2="HSV" labels3="XYZ" labels4="Yxy" labels5="Lab" labels6="OKLab" labels7="LCh" labels8="OKLCh" labels9="CMC" value="5"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="colour">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Value"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Colour_new_item.Widget_colour_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="U6">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
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
              <Row name="tint">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Tint" from="0" to="1" value="0.5337078651685393"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_colourize_item.action U2 U5"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="7478" y="5" open="true" selected="false" sform="false" next="4" name="V" caption="Filter / Browse">
      <Subcolumn vislevel="3">
        <Row popup="false" name="V1">
          <Rhs vislevel="0" flags="4">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="V2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="image">
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
              <Row name="band">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Band" from="0" to="2" value="1.3999999999999999"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="display">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Display as" labelsn="7" labels0="Grey" labels1="Green over Red" labels2="Blue over Red" labels3="Red over Green" labels4="Red over Blue" labels5="Blue over Green" labels6="Green over Blue" value="3"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_browse_multiband_item.Bandwise_item.action V1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="V3">
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
              <Row name="bit">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Bit" from="0" to="7" value="4"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_browse_multiband_item.Bitwise_item.action V1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="7962" y="5" open="true" selected="false" sform="false" next="9" name="W" caption="Filter / Photographic">
      <Subcolumn vislevel="3">
        <Row popup="false" name="W1">
          <Rhs vislevel="0" flags="4">
            <iImage window_x="965" window_y="75" window_width="547" window_height="727" show_status="true" show_convert="true"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="W2">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_negative_item.action W1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="W3">
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
              <Row name="kink">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Kink" from="0" to="1" value="0.30000000000000004"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_solarize_item.action W1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="W4">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="4" window_y="53" window_width="512" window_height="729" show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
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
              <Row name="r">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Radius" from="0" to="50" value="32.869080779944291"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="highlights">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Highlights" from="0" to="100" value="96.378830083565461"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="glow">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Glow" from="0" to="1" value="0.91922005571030641"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="colour">
                <Rhs vislevel="3" flags="7">
                  <Colour colour_space="sRGB" value0="236" value1="17" value2="22"/>
                  <Subcolumn vislevel="1"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_diffuse_glow_item.action W1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="W5">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="4" window_y="53" window_width="570" window_height="750" show_status="true" show_convert="true"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_drop_shadow_item.action W1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="W6">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="8" window_y="76" window_width="636" window_height="729" show_status="true" show_convert="true" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1">
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="text">
                <Rhs vislevel="1" flags="1">
                  <Subcolumn vislevel="0"/>
                  <iText/>
                  <String caption="Text to paint" value="&lt;span color=&quot;red&quot;&gt;Hello&lt;/span&gt; world!"/>
                </Rhs>
              </Row>
              <Row name="font">
                <Rhs vislevel="1" flags="1">
                  <Subcolumn vislevel="0"/>
                  <iText/>
                  <Fontname caption="Use font" value="Sans 12"/>
                </Rhs>
              </Row>
              <Row name="align">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="dpi">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="DPI"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="500"/>
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
              <Row name="place">
                <Rhs vislevel="1" flags="1">
                  <iRegion show_status="false" show_convert="false">
                    <iRegiongroup/>
                  </iRegion>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="justify">
                <Rhs vislevel="1" flags="1">
                  <Toggle caption="Justify" value="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="autofit">
                <Rhs vislevel="1" flags="1">
                  <Toggle caption="Fit to box" value="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="spacing">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Line spacing"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="wrap">
                <Rhs vislevel="1" flags="1">
                  <Toggle caption="Wrap lines on character boundaries" value="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="x">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_paint_text_item.action W1"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="4580" y="5" open="true" selected="false" sform="false" next="8" name="A" caption="Filter / Spatial Correlation">
      <Subcolumn vislevel="3">
        <Row popup="false" name="A1">
          <Rhs vislevel="0" flags="4">
            <iImage window_x="1082" window_y="29" window_width="547" window_height="1161" show_status="true" show_convert="true"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A4">
          <Rhs vislevel="1" flags="1">
            <iImage window_x="1406" window_y="463" window_width="510" window_height="727" show_status="true" show_convert="true"/>
            <Subcolumn vislevel="0"/>
            <iText formula="A1?1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A5">
          <Rhs vislevel="1" flags="1">
            <iRegion show_status="false" show_convert="false">
              <iRegiongroup/>
            </iRegion>
            <Subcolumn vislevel="0"/>
            <iText formula="Region A4 165 82 70 57"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A6">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="477" window_y="232" window_width="510" window_height="727" show_status="true" show_convert="true" scale="255.00000000000043" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_correlate_item.Correlate_item.action A4 A5"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A7">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="867" window_y="355" window_width="510" window_height="727" show_status="true" show_convert="true" scale="2.7433436451138365e-06" offset="0.5" page="0" falsecolour="true" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_correlate_item.Correlate_fast_item.action A4 A5"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="5053" y="5" open="true" selected="true" sform="false" next="7" name="D" caption="Filter / Hough">
      <Subcolumn vislevel="3">
        <Row popup="false" name="D1">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D4">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="D1?1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="255.00000000000043" offset="0" page="0" falsecolour="false" mode="multipage"/>
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
              <Row name="sigma">
                <Rhs vislevel="1" flags="1">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="prec">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Precision" labelsn="3" labels0="Integer" labels1="Float" labels2="Approximate" value="0"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_conv_item.Canny_item.action D4"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D5">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_hough_item.Line_item.action D2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D6">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false" scale="1" offset="0" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="1">
              <Row name="a">
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
              <Row name="scale">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Scale down parameter space by"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="min_radius">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Minimum radius"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="max_radius">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Maximum radius"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="40"/>
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
            <iText formula="Filter_hough_item.Circle_item.action D2"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
  </Workspace>
</root>
