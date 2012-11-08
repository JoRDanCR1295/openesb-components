/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)SVGRenderer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.renderers.svg;

import com.sun.jbi.cam.manager.framework.common.JBIServiceInformation;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import com.sun.jbi.cam.common.resources.Messages;

/**
 * @author Seebeyond
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class SVGRenderer extends SVGJavaScript {

    private Logger logger = 
            Logger.getLogger(SVGRenderer.class.getName());

    private static String VISIBILITY_STATE = "LEGEND_VISIBILITY_STATE";
    private static String VISIBLE_STATE = "'visible'";
    private static String HIDDEN_STATE = "'hidden'";
    
    private static String SVG_HEADER =
         "<svg onload=\"init(evt)\" " +
         "xmlns=\"http://www.w3.org/2000/svg\" "+
        "xmlns:xlink=\"http://www.w3.org/1999/xlink\" " +
      " width=\"100%\" height=\"100%\"> " +"\n\n" ;
    
    // this is the def. for the tooltip element used
    // with the mouse over event
    private static String SVG_TOOLTIP_BLOCK = 
       "<g id='ToolTip' opacity='0.8' visibility='hidden' pointer-events='none'>  \n" +
         "<rect id='tipbox' x='0' y='5' width='88' height='20' rx='2' ry='2' fill='white' stroke='black'/>  \n" +
           "<text id='tipText' x='5' y='20' font-family='Arial' font-size='12'>  \n" +
             "<tspan id='tipTitle0' x='5' font-weight='bold'><![CDATA[]]></tspan> \n" + 
             "<tspan id='tipTitle' x='5' dy='12' font-weight='bold'><![CDATA[]]></tspan>  \n" +
             "<tspan id='tipTitle2' x='5' dy='12' font-weight='bold'><![CDATA[]]></tspan>  \n" +
             "<tspan id='tipDesc' x='5' dy='15' fill='blue'><![CDATA[]]></tspan>  \n" +
           "</text> \n" + 
       "</g>"; 
        
    private static String SERVICE_UNIT_STATISTIC_TOOL_TIP_BLOCK = 
    "<g id='statisticToolTip' opacity='0.8' visibility='hidden' pointer-events='none'>  \n" + 
    "<rect id='statisticTipbox' x='0' y='5' width='88' height='400' rx='2' ry='2' fill='white' stroke='black'/>  \n" + 
    "<text id='statisticTipText' x='5' y='20' font-family='Arial' font-size='12'>   \n" +
      "<tspan id='statisticTipDesc' x='5'  fill='blue'><![CDATA[]]></tspan>    \n" + 
      "<tspan id='statisticTipTitle0' x='5' dy='12' font-weight='bold' ><![CDATA[]]></tspan>  \n" +
      "<tspan id='statisticTipTitle1' x='5' dy='12' font-weight='bold'><![CDATA[]]></tspan>  \n" + 
      "<tspan id='statisticTipTitle2' x='5' dy='12' font-weight='bold'><![CDATA[]]></tspan>  \n" +
      "<tspan id='statisticTipTitle3' x='5' dy='12' font-weight='bold'><![CDATA[]]></tspan>  \n" +
      "<tspan id='statisticTipTitle4' x='5' dy='12' font-weight='bold'><![CDATA[]]></tspan>  \n" +
      "<tspan id='statisticTipTitle5' x='5' dy='12' font-weight='bold'><![CDATA[]]></tspan>  \n" + 
      "<tspan id='statisticTipTitle6' x='5' dy='12' font-weight='bold'><![CDATA[]]></tspan>  \n" + 
      "<tspan id='statisticTipTitle7' x='5' dy='12' font-weight='bold'><![CDATA[]]></tspan>  \n" + 
      "</text>   \n" + 
     "</g>  \n";
        
    private static String LEGEND_BLOCK = 
        "  <g id='legend'  pointer-events='none'  visibility=" + VISIBILITY_STATE + " >\n" +
        "   <rect id='legendbox'  x='0' y='5' width='200' height='130' rx='2' ry='2' fill='white' stroke='black' stroke-width='2'/>\n" +
        "     <text id='legendText' x='100' y='20' font-family='Arial' font-size='12'>\n" +
        "        <tspan id='slegendTitle' x='70' font-weight='bold'>" + 
              Messages.getString("svg.legend") + "</tspan>\n" +
        "     </text>\n" +
        "     <circle id='consumeendpoint' cx='10' cy='40' r='2' fill='#FF0000' /> \n" +
        "     <text id='consumeendpointText' x='40' y='45' font-family='Arial' font-size='12'>\n" +
        "	 <tspan id='consumeendpointTitle' x='40' font-weight='bold'>" + 
              Messages.getString("svg.consumeendpoint") + "</tspan>\n" +
        "     </text>\n" +
        "     <path id='providesendpoint' d=' M9,55 L12,58 L9,61Z' fill='#CCCCCC' stroke='black' stroke-width='1' />\n" +
        "     <text id='providesendpointText' x='40' y='63' font-family='Arial' font-size='12'>\n" +
        "	<tspan id='providesendpointTitle' x='40' font-weight='bold'>" +
              Messages.getString("svg.providesendpoint") + "</tspan>\n" +
        "     </text>\n" +
        "     <line id='bc2seconn'  x1='10' y1='80' x2='20' y2='80' stroke='#A040C0' stroke-width='2'/>\n" +
        "     <text id='bc2seconnText' x='40' y='85' font-family='Arial' font-size='12'>\n" +
        "	<tspan id='bc2seconnTitle' x='40' font-weight='bold'>" +
            Messages.getString("svg.bc2se") + "</tspan>\n" +
        "     </text>\n" +
        "     <line id='se2bcconn'  x1='10' y1='95' x2='20' y2='95' stroke='#00FF00' stroke-width='2'/>\n" +
        "     <text id='se2bcconnText' x='40' y='100' font-family='Arial' font-size='12'>\n" +
        "	<tspan id='se2bcconnTitle' x='40' font-weight='bold'>" +
            Messages.getString("svg.se2bc") + "</tspan>\n" +
        "     </text>\n" +
        "     <line id='se2seconn'  x1='10' y1='110' x2='20' y2='110' stroke='#FF0000' stroke-width='2'/>\n" +
        "     <text id='se2seconnText' x='40' y='115' font-family='Arial' font-size='12'>\n" +
        "       <tspan id='se2seconnTitle' x='40' font-weight='bold'>" +
            Messages.getString("svg.se2se") + "</tspan>\n" +
        "     </text>\n" +
        "     <line id='bc2bcconn'  x1='10' y1='125' x2='20' y2='125' stroke='#0000FF' stroke-width='2'/>\n" +
        "     <text id='bc2bcconnText' x='40' y='130' font-family='Arial' font-size='12'>\n" +
        "	<tspan id='bc2bcconnTitle' x='40' font-weight='bold'>" + 
            Messages.getString("svg.bc2bc") + "</tspan>\n" +
        "     </text>\n" +
        "</g>\n";
            
        
    public static String SVG_END_TAG = "</svg>";
    public static String G_START_TAG = "<g id=\"serviceUnitContainer\">";
    public static String G_END_TAG = "</g>";
    public static final int INITIAL_X_OFFSET = 50;
    public static final int INITIAL_Y_OFFSET = 50;
    public static final int UNIT_VARTICAL_GAP_SIZE = 100;
    public static final int UNIT_HORIZONTAL_GAP_SIZE = 200;
    public static Point SERVICE_BC_UNIT_START = 
        new Point(INITIAL_X_OFFSET,INITIAL_Y_OFFSET);
    public static Point SERVICE_SE_UNIT_START = new Point(INITIAL_X_OFFSET + 
            			UNIT_HORIZONTAL_GAP_SIZE,INITIAL_Y_OFFSET);
    public static int SERVICE_UNIT_WIDTH = 50;

    public static int DEFAULT_SERVICE_UNIT_HEIGHT = 100;
    // used in conjuction with consumes point count to define the final hieght
    // of the service unit
    public static int CONSUMES_UNIT_HEIGHT_OFFSET = 8;
    // used in conjuction with provides point count to define the final hieght
    // of the service unit
    public static int PROVIDES_UNIT_HEIGHT_OFFSET = 12;
    

    	
    private boolean isLegendVisible;
    private String svgFileName;
    private String targetName;
    /**
     * @param fileName - The name of the file that contain the generate
     * SVG xml. 
     */
    public SVGRenderer(String fileName,boolean isLegendVisible,
            String targetName) {
        super();
        svgFileName = fileName;
        this.isLegendVisible = isLegendVisible;
        this.targetName = targetName;
    }

    
    /**
     * start the process of generating the SVG and store in the file name
     * provided in the constructor.
     * 
     * @param jbiServiceInformationList - contains the metadata of the 
     *                                    service unit
     * @param jbiConnectionsInformation - contains the metadata of the 
     *                                    connection between the service units
     * @throws Exception if it fails to generate the xml or the write the 
     *                   information to the file. 
     */
    public void generateSVGFile(List jbiServiceInformationList,
            JBIConnectionsInformation jbiConnectionsInformation) 
            throws Exception{
        
        FileWriter writer = new FileWriter("/"+svgFileName+".svg");
        writer.write(generateSVG(jbiServiceInformationList,
                jbiConnectionsInformation));
        writer.flush();
        writer.close();
    }
    
    
    /**
     * generate a string object that contain the xml data that makes up the 
     * svg. it is fed to the ASV that renders it.
     * 
     * @param jbiServiceInformationList - contains the metadata of the 
     *                                    service unit
     * @param jbiConnectionsInformation - contains the metadata of the 
     *                                    connection between the service units
     * @return string represent the svg data.
     * @throws Exception if it fails to generate the xml
     */
    public String generateSVG(List jbiServiceInformationList,
            JBIConnectionsInformation jbiConnectionsInformation) 
            throws Exception{
        
        StringBuffer buffer = new StringBuffer();
        buffer.append(SVG_HEADER + SCRIPT_BLOCK);
        addServicesAndConnections(jbiServiceInformationList,
                jbiConnectionsInformation,buffer);
        buffer.append(SVG_TOOLTIP_BLOCK);
        buffer.append(SERVICE_UNIT_STATISTIC_TOOL_TIP_BLOCK);
        buffer.append(updateLegendVisibilityAttribute());
        buffer.append(SVG_END_TAG);
        return buffer.toString();
        
    }

    private void addServicesAndConnections(List jbiServiceInformationList,
            JBIConnectionsInformation jbiConnectionsInformation,
            StringBuffer buffer) {
        List <SVGServiceUnit> lBCUnitList =
                    new ArrayList<SVGServiceUnit>();
        List <SVGServiceUnit> lSEUnitList = 
                new ArrayList<SVGServiceUnit>();
        int indexBC = 0;
        int indexSE = 0;
        
        Rectangle rectBC = new Rectangle(SERVICE_BC_UNIT_START,
                        new Dimension(SERVICE_UNIT_WIDTH,
                                      DEFAULT_SERVICE_UNIT_HEIGHT));
        Rectangle rectSE = new Rectangle(SERVICE_SE_UNIT_START,
                        new Dimension(SERVICE_UNIT_WIDTH,
                                      DEFAULT_SERVICE_UNIT_HEIGHT));
        buffer.append(G_START_TAG);
        for (Iterator iter = jbiServiceInformationList.iterator(); iter
                .hasNext();) {
            JBIServiceInformation service = (JBIServiceInformation) iter.next();
            boolean isBindComponent = service.isBindComponent();
            Rectangle rect = isBindComponent==true ? rectBC :rectSE; 

            SVGServiceUnit unit = new SVGServiceUnit(service,
                                      jbiConnectionsInformation,
                                      rect.x,rect.y,
                                      rect.width,rect.height,
                                      isBindComponent,
                                      isBindComponent==true ? ++indexBC : 
                                      ++indexSE);
            
                        
            if(isBindComponent==true) {
                lBCUnitList.add(unit);
            } else {
                 lSEUnitList.add(unit);
           }
        }
        
        // recalc units rectangle taking into account the number of connection
        // point in each unit.
        
        recalcServiceUnitsRect(lBCUnitList,lSEUnitList);
        // insure that BC processed prior to SE
         List<SVGServiceUnit> serviceUnits = 
                 new ArrayList<SVGServiceUnit>(lBCUnitList);
         serviceUnits.addAll(lSEUnitList);
         // make all service unit aware of each other
        //  in order to process connection drawing correctly
        for (Iterator iter = serviceUnits.iterator(); iter
                        .hasNext();) {
            SVGServiceUnit serviceUnit = (SVGServiceUnit) iter.next();
            serviceUnit.setServiceUnitList(serviceUnits);
        }
        
        Map<SVGConnectionSegments,SVGConnectionSegments> connectionsMap = null;
        for (Iterator iter = serviceUnits.iterator(); iter.hasNext();) {
            SVGServiceUnit serviceUnit = (SVGServiceUnit) iter.next();
            serviceUnit.AddService(buffer);
            // propogate existing connection maps to the next SU
            // will be used to prevent connection lines overlaps.
            if(connectionsMap != null && !connectionsMap.isEmpty()) {
                serviceUnit.setConnectionsSegmentMap(connectionsMap);
            }
            serviceUnit.addConnections(buffer);
            
            connectionsMap =  serviceUnit.getConnectionsSegmentMap();
        }
        
        buffer.append(G_END_TAG);
        
    }
    
    private void  recalcServiceUnitsRect(List<SVGServiceUnit> aBCUnitList ,
            List<SVGServiceUnit> aSEUnitList ) {
        
        // get max size of the maps. it defines the number of time we will
        // loop
        int count =  Math.max(aBCUnitList.size(),aSEUnitList.size());
        // get all bc comsumes Point count will be used to define the 
        // distance between BC's and SE's
        int horizontalSUGaps = calcHorizontalGap(aBCUnitList);

        for(int index = 0; index < count ; index ++) {
 //           Integer currentIndex =  new Integer(index);
            SVGServiceUnit bcSVGServiceUnit = null;
            SVGServiceUnit seSVGServiceUnit = null;
            
            if(aBCUnitList.size()-1 >= index) {
                bcSVGServiceUnit= aBCUnitList.get(index);
            }
            if(aSEUnitList.size()-1 >= index) {
               seSVGServiceUnit= aSEUnitList.get(index);
            }
            // the numbers of BC & SE often not the same
            // therefore we need to check the return instnace for null value
            Rectangle unitRect = null;
            if(bcSVGServiceUnit != null) {
               unitRect =  bcSVGServiceUnit.getThisUnitRect(); 
               readjectSVGUnitRect(bcSVGServiceUnit.getJBIServiceInformation(),
                   unitRect,index,aBCUnitList, aSEUnitList);
               bcSVGServiceUnit.setThisUnitRect(unitRect); 
                  
            }   
            if(seSVGServiceUnit != null) {
               unitRect =  seSVGServiceUnit.getThisUnitRect(); 
               readjectSVGUnitRect(seSVGServiceUnit.getJBIServiceInformation(),
                    unitRect,index,aBCUnitList, aSEUnitList);
               // the following will insure that all bc consume connection 
               // will be drawn between BC's && SE's
               unitRect.x = INITIAL_X_OFFSET + SERVICE_UNIT_WIDTH + horizontalSUGaps;
               seSVGServiceUnit.setThisUnitRect(unitRect);
            }
        }
        
    }
     
    private void readjectSVGUnitRect(JBIServiceInformation service, 
            Rectangle rect, int unitIndex, 
            List<SVGServiceUnit> aBCUnitList ,
            List<SVGServiceUnit> aSEUnitList ) {
        
        SVGServiceUnit bcSVGServiceUnit = null;
        SVGServiceUnit seSVGServiceUnit = null;
        
        int consumesCount = service.getConsumesMap().size();
        int providesCount = service.getProvidesMap().size();
        int newRectheight = Math.max(consumesCount * CONSUMES_UNIT_HEIGHT_OFFSET,
                            providesCount * PROVIDES_UNIT_HEIGHT_OFFSET );
        newRectheight = Math.max(newRectheight, DEFAULT_SERVICE_UNIT_HEIGHT); 
        rect.setSize(rect.width,newRectheight);
        // no need to adjust the starting point for 
        // the first unit (BC or SE)
        if(unitIndex == 0) {
           return;
        } else {
            // the start point need to be adjusted based on the 
            // previous SU start point and height
            int previousIndex =  unitIndex-1;
            if(aBCUnitList.size()-1 >= previousIndex) {
                bcSVGServiceUnit= aBCUnitList.get(previousIndex);
            }
            
            if(aSEUnitList.size()-1 >= previousIndex) {
                seSVGServiceUnit= aSEUnitList.get(previousIndex);
            }
           
            if(bcSVGServiceUnit == null) {
                // no more BC calc next SE start point (y value)based on 
                // last SE only.
                Rectangle lastRect = seSVGServiceUnit.getThisUnitRect();
                rect.y = lastRect.y + lastRect.height + UNIT_VARTICAL_GAP_SIZE;
            } else if(seSVGServiceUnit == null) {
                // no more SE calc next BC start point (y value) based on 
                // last BC only.
                Rectangle lastRect = bcSVGServiceUnit.getThisUnitRect();
                rect.y = lastRect.y + lastRect.height + UNIT_VARTICAL_GAP_SIZE;
            } else {
                // both SE and BC are valid use the max of both
                // for the next start point (y value). 
                   Rectangle lastSERect = seSVGServiceUnit.getThisUnitRect();
                   Rectangle lastBCRect = bcSVGServiceUnit.getThisUnitRect();
                   int maxY = Math.max(lastSERect.y + lastSERect.height,
                                       lastBCRect.y + lastSERect.height);
                   rect.y = maxY + UNIT_VARTICAL_GAP_SIZE;
            }
        }
        
    }
    
    private int calcHorizontalGap(List<SVGServiceUnit> aBCUnitList) {
        int gap = 0;
        for (Iterator iter = aBCUnitList.iterator(); iter.hasNext();) {
            SVGServiceUnit su = (SVGServiceUnit) iter.next();
            gap += su.getJBIServiceInformation().getConsumesMap().size();
        }
        // adding 2 extra lines offset
        return Math.max((gap+2)*SVGConnectionSegments.LINE_OFFSET_SIZE,UNIT_HORIZONTAL_GAP_SIZE);
    }
    
    private String updateLegendVisibilityAttribute() {
        String legend = LEGEND_BLOCK;
        legend = legend.replace(VISIBILITY_STATE,
                isLegendVisible==true? VISIBLE_STATE : HIDDEN_STATE );
        return legend;
    }
    
    public static void main(String[] args) {
    }
}
