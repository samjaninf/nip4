<?xml version="1.0"?>
<root xmlns="http://www.vips.ecs.soton.ac.uk/nip/9.1.0">
  <Workspace view="WORKSPACE_MODE_REGULAR" scale="1" offset="0" locked="false" local_defs="// local definitions for this tab&#10;" name="tab2" filename="$CWD/logo-nip4.ws" major="9" minor="1">
    <Column x="10" y="5" open="true" selected="false" sform="false" next="14" name="A" caption="render text">
      <Subcolumn vislevel="3">
        <Row popup="false" name="A12">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="text">
                <Rhs vislevel="2" flags="6">
                  <Subcolumn vislevel="1">
                    <Row name="rgba">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="message">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="super">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="text">
                      <Rhs vislevel="1" flags="1">
                        <String caption="Text to paint" value="VIPS"/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="font">
                      <Rhs vislevel="1" flags="1">
                        <Fontname caption="Use font" value="DejaVu Sans Bold 12"/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
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
                              <iText formula="1000"/>
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
                    <Row name="spacing">
                      <Rhs vislevel="1" flags="1">
                        <Expression caption="Line spacing"/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="wrap">
                      <Rhs vislevel="1" flags="1">
                        <Toggle/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="justify">
                      <Rhs vislevel="1" flags="1">
                        <Toggle/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="autodpi">
                      <Rhs vislevel="1" flags="1">
                        <Toggle/>
                        <Subcolumn vislevel="0"/>
                        <iText/>
                      </Rhs>
                    </Row>
                  </Subcolumn>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="nwidth">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Image width (pixels)"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="nheight">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Image height (pixels)"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_new_item.Text_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A2">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="662" window_y="411" window_width="684" window_height="469" show_status="true" show_convert="true"/>
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
              <Row name="r">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Radius" from="1" to="100" value="22"/>
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
            <iText formula="Filter_conv_item.Custom_blur_item.action A12"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A4">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="431" window_y="362" window_width="963" window_height="641" show_status="true" show_convert="true"/>
            <Subcolumn vislevel="0"/>
            <iText formula="A12 - A2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A6">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.U8_item.action A4"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A8">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Colour_convert_item.sRGB_item.action A6"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A11">
          <Rhs vislevel="3" flags="7">
            <iImage window_x="632" window_y="366" window_width="915" window_height="592" show_status="true" show_convert="false"/>
            <Subcolumn vislevel="0">
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
                  <Slider caption="Radius" from="0" to="50" value="14.000000000000002"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="highlights">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Highlights" from="0" to="100" value="83"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="glow">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Glow" from="0" to="1" value="0.90000000000000002"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="colour">
                <Rhs vislevel="3" flags="7">
                  <Colour colour_space="Lab" value0="78.429710388183594" value1="10.576605796813965" value2="74.185394287109375"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_diffuse_glow_item.action A8"/>
          </Rhs>
        </Row>
        <Row popup="false" name="A13">
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
              <Row name="r">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Radius" from="0" to="50" value="14.000000000000002"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="highlights">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Highlights" from="0" to="100" value="83"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="glow">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Glow" from="0" to="1" value="0.90000000000000002"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="colour">
                <Rhs vislevel="3" flags="7">
                  <Colour colour_space="LAB" value0="71.953727722167969" value1="12.948423385620117" value2="74.347236633300781"/>
                  <Subcolumn vislevel="1"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Filter_diffuse_glow_item.action A8"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="627" y="5" open="true" selected="false" sform="false" next="22" name="B" caption="add texture">
      <Subcolumn vislevel="3">
        <Row popup="false" name="B15">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="A13"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B16">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <iImage show_status="false" show_convert="false"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="nsize">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Image size (pixels)"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="256"/>
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
              <Row name="dimension">
                <Rhs vislevel="1" flags="1">
                  <Slider caption="Dimension" from="2.0009999999999999" to="2.9990000000000001" value="2.6022950000000007"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_new_item.Noise_item.Fractal_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B17">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_levels_item.Scale_item.action B16"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B2">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_enhance_item.Falsecolour_item.action B17"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B18">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1">
              <Row name="default_type">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
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
              <Row name="across">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Tiles across"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="1 + B15.width / B16.width"/>
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
              <Row name="down">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Tiles down"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="1 + B15.height / B16.height"/>
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
              <Row name="repeat">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_tile_item.Replicate_item.action B2"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B19">
          <Rhs vislevel="3" flags="7">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Filter_blend_item.Scale_blend_item.action B18 B15"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B20">
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
              <Row name="nleft">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Crop left"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="0"/>
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
              <Row name="ntop">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Crop top"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="0"/>
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
              <Row name="nwidth">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Crop width"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="B15.width"/>
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
              <Row name="nheight">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Crop height"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="B15.height"/>
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
            <iText formula="Image_crop_item.action B19"/>
          </Rhs>
        </Row>
        <Row popup="false" name="B21">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Image_number_format_item.U8_item.action B20"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1133" y="5" open="true" selected="false" sform="false" next="27" name="C" caption="make shadow">
      <Subcolumn vislevel="3">
        <Row popup="false" name="C7">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="A12"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C9">
          <Rhs vislevel="1" flags="1">
            <Slider caption="shadow fuzziness" from="0" to="100" value="38.3459"/>
            <Subcolumn vislevel="0">
              <Row name="caption">
                <Rhs vislevel="0" flags="4">
                  <iText formula="&quot;shadow fuzziness&quot;"/>
                </Rhs>
              </Row>
              <Row name="from">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="to">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="value">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <Real/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Widget_slider_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C15">
          <Rhs vislevel="1" flags="1">
            <Slider caption="shadow scale" from="1" to="2" value="1.1014999999999999"/>
            <Subcolumn vislevel="0">
              <Row name="caption">
                <Rhs vislevel="0" flags="4">
                  <iText formula="&quot;shadow scale&quot;"/>
                </Rhs>
              </Row>
              <Row name="from">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="to">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="value">
                <Rhs vislevel="0" flags="4">
                  <iText/>
                </Rhs>
              </Row>
              <Row name="super">
                <Rhs vislevel="0" flags="4">
                  <Real/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Widget_slider_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C19">
          <Rhs vislevel="1" flags="4">
            <iText formula="&quot;shadow colour&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C14">
          <Rhs vislevel="3" flags="7">
            <Colour colour_space="sRGB" value0="148" value1="112" value2="94"/>
            <Subcolumn vislevel="1"/>
            <iText formula="Colour_new_item.Widget_colour_item.action"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C25">
          <Rhs vislevel="1" flags="4">
            <iText formula="&quot;background colour&quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C17">
          <Rhs vislevel="3" flags="7">
            <iText formula="Colour_new_item.Widget_colour_item.action"/>
            <Colour colour_space="sRGB" value0="255" value1="255" value2="255"/>
            <Subcolumn vislevel="1"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C11">
          <Rhs vislevel="1" flags="1">
            <iText formula="Expression &quot;offset shadow vertically by&quot; (-25)"/>
            <Expression caption="offset shadow vertically by"/>
            <Subcolumn vislevel="0"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C20">
          <Rhs vislevel="1" flags="4">
            <iText formula="&quot; &quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C22">
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
              <Row name="nwidth">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="New width (pixels)"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="C7.width + 2 * C9.value"/>
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
              <Row name="nheight">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="New height (pixels)"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="C7.height + 2 * C9.value"/>
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
              <Row name="bgcolour">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Background colour"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="position">
                <Rhs vislevel="1" flags="1">
                  <Option/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="left">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Pixels from left"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="top">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Pixels from top"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_transform_item.Resize_item.Resize_item.Resize_item.Resize_canvas_item.action C7"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C26">
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
              <Row name="r">
                <Rhs vislevel="2" flags="5">
                  <Slider/>
                  <Subcolumn vislevel="0"/>
                  <iText formula="C9"/>
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
            <iText formula="Filter_conv_item.Custom_blur_item.action C22"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C23">
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
              <Row name="xfactor">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Horizontal scale factor"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="C15.value"/>
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
              <Row name="yfactor">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Vertical scale factor"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="C15.value"/>
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
              <Row name="kernel">
                <Rhs vislevel="2" flags="5">
                  <Subcolumn vislevel="0"/>
                  <iText/>
                  <Option/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_transform_item.Resize_item.Scale_item.action C26"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C18">
          <Rhs vislevel="1" flags="1">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_blend_item.Image_blend_item.action C23 C14 C17"/>
          </Rhs>
        </Row>
        <Row popup="false" name="C24">
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
              <Row name="nwidth">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="New width (pixels)"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="nheight">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="New height (pixels)"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="C18.height + C11.expr"/>
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
              <Row name="bgcolour">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Background colour"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="C17"/>
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
              <Row name="position">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Position" labelsn="10" labels0="North-west" labels1="North" labels2="North-east" labels3="West" labels4="Centre" labels5="East" labels6="South-west" labels7="South" labels8="South-east" labels9="Specify in pixels" value="7"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="left">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Pixels from left"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="top">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Pixels from top"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_transform_item.Resize_item.Resize_item.Resize_item.Resize_canvas_item.action C18"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
    <Column x="1793" y="5" open="true" selected="true" sform="false" next="19" name="D" caption="position and blend layers">
      <Subcolumn vislevel="3">
        <Row popup="false" name="D10">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="A12"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D9">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="B21"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D8">
          <Rhs vislevel="2" flags="5">
            <iImage show_status="false" show_convert="false"/>
            <Subcolumn vislevel="0"/>
            <iText formula="C24"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D11">
          <Rhs vislevel="1" flags="4">
            <iText formula="&quot; &quot;"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D14">
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
              <Row name="nwidth">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="New width (pixels)"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="D8.width"/>
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
              <Row name="nheight">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="New height (pixels)"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="D8.height"/>
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
              <Row name="bgcolour">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Background colour"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="position">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Position" labelsn="10" labels0="North-west" labels1="North" labels2="North-east" labels3="West" labels4="Centre" labels5="East" labels6="South-west" labels7="South" labels8="South-east" labels9="Specify in pixels" value="1"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="left">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Pixels from left"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="top">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Pixels from top"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_transform_item.Resize_item.Resize_item.Resize_item.Resize_canvas_item.action D10"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D15">
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
              <Row name="nwidth">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="New width (pixels)"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="D8.width"/>
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
              <Row name="nheight">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="New height (pixels)"/>
                  <Subcolumn vislevel="0">
                    <Row name="caption">
                      <Rhs vislevel="0" flags="4">
                        <iText/>
                      </Rhs>
                    </Row>
                    <Row name="expr">
                      <Rhs vislevel="0" flags="4">
                        <iText formula="D8.height"/>
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
              <Row name="bgcolour">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Background colour"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="position">
                <Rhs vislevel="1" flags="1">
                  <Option caption="Position" labelsn="10" labels0="North-west" labels1="North" labels2="North-east" labels3="West" labels4="Centre" labels5="East" labels6="South-west" labels7="South" labels8="South-east" labels9="Specify in pixels" value="1"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="left">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Pixels from left"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
              <Row name="top">
                <Rhs vislevel="1" flags="1">
                  <Expression caption="Pixels from top"/>
                  <Subcolumn vislevel="0"/>
                  <iText/>
                </Rhs>
              </Row>
            </Subcolumn>
            <iText formula="Image_transform_item.Resize_item.Resize_item.Resize_item.Resize_canvas_item.action D9"/>
          </Rhs>
        </Row>
        <Row popup="false" name="D16">
          <Rhs vislevel="2" flags="5">
            <iImage window_x="602" window_y="662" window_width="985" window_height="382" show_status="true" show_convert="false" scale="1.44462" offset="-128" page="0" falsecolour="false" mode="multipage"/>
            <Subcolumn vislevel="0"/>
            <iText formula="Filter_blend_item.Image_blend_item.action D14 D15 D8"/>
          </Rhs>
        </Row>
      </Subcolumn>
    </Column>
  </Workspace>
</root>
