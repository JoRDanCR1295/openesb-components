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
 * @(#)SVGServiceUnit.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.renderers.svg;

import com.sun.jbi.cam.manager.framework.common.JBIServiceInformation;
import com.sun.jbi.cam.manager.framework.common.JBIServiceUnitInformation;
import java.awt.Rectangle;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.logging.Logger;
import com.sun.jbi.cam.common.resources.Messages;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.Util;
import com.sun.jbi.cam.manager.framework.core.ComponentServiceProviderResolver;

/**
 * @author Sun MicrosystemInc.
 *
 * the class represent a service unit. It responsible for generating the
 * svg drawing by creating the service unit rectangle the endpoints and 
 * the connectivity between them.  
 * 
 */
/**
 * @author rdamir
 *
 */

public class SVGServiceUnit {
    
    private Logger logger = 
            Logger.getLogger(SVGServiceUnit.class.getName());
   
    private static final String COLOR_PURPLE = "#A040C0";
    private static final String COLOR_RED = "#FF0000";
    private static final String COLOR_GREEN = "#00CC00";
    private static final String COLOR_BLUE = "#0000FF";
    private static final String COLOR_BLACK = "#000000";
    private static final String COLOR_LIGHT_GRAY = "#CCCCCC";
    private static final String COLOR_DARK_GRAY = "#A0A0A0";
    private static final String COLOR_WHITE = "#FFFFFF";

    private static final int BC2SE = 0;
    private static final int SE2SE = 1;
    private static final int SE2BC = 2;
    private static final int BC2BC = 3;
    private static final int SELECTED = 4;
    // tags for svg elements  
    private static final String TITLE_START_TAG = "<title>";
    private static final String TITLE_END_TAG = "</title>";
    private static final String CIRCLE_END_TAG = "</circle>";
    private static final String PATH_END_TAG = "</path>";
    private static final String POLYLINE_END_TAG = "</polyline>";

    private static final String END_POINT_NAME = "#ENDPOINTNAME#";
    private static final String FQ_END_POINT_NAME = "#FQENDPOINTNAME#";
   // the following is a template that represent the service 
    // it is wrapped by anchor tag to allow it to launch the
    // service unit status page
    
    private static final String RECT_APP_NAME = "#RECTAPPNAME#";
    private static final String RECT_SRV_NAME = "#RECTSRVNAME#";
    private static final String RECT_COMP_NAME= "#RECTCOMPNAME#";
    private static final String RECT_SRV_TYPE = "#RECTSRVTYPE#";
    private static final String RECT_SU_NAME = "#RECTSUNAME#";
    private static final String RECT_X_POS = "#RECTXPOS#";
    private static final String RECT_Y_POS = "#RECTYPOS#";
    private static final String RECT_WIDTH = "#RECTWIDTH#";
    private static final String RECT_HIEGHT = "#RECTHIEGHT#";
    private static final String RECT_HREF = "#RECTHREF#";
    private static final String RECT_FILL = "#RECTHFILL#";
    private static final String RECT_TARGET_NAME = "#RECTTARGETNAME#";
            
    private static final String RECTANGLE_TEMPLATE = 
        "<rect id=\"" + RECT_SU_NAME +"\"  x=\"" + RECT_X_POS +"\" y=\"" +
        RECT_Y_POS+"\" width=\"" + RECT_WIDTH+"\"  height=\"" + RECT_HIEGHT +
        "\"  fill=\"" + RECT_FILL +"\" stroke-width=\"1\" stroke=\"" + 
        COLOR_BLACK+ "\" HREF=\"/cam" + RECT_HREF + "?" +
//            \"/cam/faces/manager/framework/generic/tabsFrame.jsp?" +
        GenericConstants.SVG_ELEMENT_TYPE + "=" + GenericConstants.SERVICE_UNIT_TYPE + "&amp;" +    
        GenericConstants.COMPONENT_NAME + "=" +  RECT_SRV_NAME + "&amp;" +
        GenericConstants.COMPONENT_TYPE + "=" + GenericConstants.SU_TYPE + "&amp;" + 
        GenericConstants.COMPONENT_CNAME + "=" +  RECT_COMP_NAME + "&amp;" +
        GenericConstants.COMPONENT_CTYPE + "=" + RECT_SRV_TYPE + "&amp;" +
        GenericConstants.COMPONENT_PNAME + "=" + RECT_APP_NAME + "&amp;" +
        GenericConstants.COMPONENT_TNAME + "=" + RECT_TARGET_NAME + "\"/>\n";

    // the following is a template that represent a consumes 
    // end point. it contain token that a replaced
    // by the correct value  in the drawConsumesPoints(..) method below
    private static final String CIRCLE_ID = "#CIRCLEID#";
    private static final String CIRCLE_X = "#CIRCLEX# ";
    private static final String CIRCLE_Y = "#CIRCLEY#";
    private static final String SVG_CIRCLE_TEMPLATE = 
        "<circle id=\"" + CIRCLE_ID + "\""  +
        " HREF=\"/cam/faces/manager/framework/generic/tabsFrame.jsp?" +
        GenericConstants.SVG_ELEMENT_TYPE + "=" + GenericConstants.CONSUMING_ID + "&amp;" +    
        GenericConstants.ENDPOINT_KEY + "=" +  END_POINT_NAME + "&amp;" +
        GenericConstants.FQ_ENDPOINT_NAME + "=" +  FQ_END_POINT_NAME + "&amp;" +
        GenericConstants.COMPONENT_NAME + "=" +  RECT_SRV_NAME + "&amp;" +
        GenericConstants.COMPONENT_TNAME + "=" + RECT_TARGET_NAME + "&amp;" +    
        GenericConstants.COMPONENT_TYPE + "=" + RECT_SRV_TYPE + "\" " + 
        "cx=\""+ CIRCLE_X+ "\" cy=\"" + CIRCLE_Y +  "\" r=\"2\" fill=\"" + 
        COLOR_RED+"\" > \n";
    // the following is a template that represent the title element  
    // for each the the following svg element circle,path & polyline
    // it used to generate tooltip window when the mouse is over the
    // svg element
    private static final String TITLE_VALUE = "#TITLEVALUE#";
    private static final String SVG_TITLE_TEMPLATE = 
        TITLE_START_TAG + TITLE_VALUE + TITLE_END_TAG;
 
    // the following is a template that represent a provides 
    // end point. it contain token that a replaced
    // by the correct value  in the drawProvidesPoints(..) method below
    private static final String TRIANGLE_ID = "#TRIANGLEID#";
    private static final String TRIANGLE_P1X = "#TRIANGLEP1X#";
    private static final String TRIANGLE_P1Y = "#TRIANGLEP1Y#";
    private static final String TRIANGLE_P2X = "#TRIANGLEP2X#";
    private static final String TRIANGLE_P2Y = "#TRIANGLEP2Y#";
    private static final String TRIANGLE_P3X = "#TRIANGLEP3X#";
    private static final String TRIANGLE_P3Y = "#TRIANGLEP3Y#";
    private static final String SVG_TRIANGLE_TEMPLATE = 
        "<path id=\"" + TRIANGLE_ID + "\"  " +
        " HREF=\"/cam/faces/manager/framework/generic/tabsFrame.jsp?" +
        GenericConstants.SVG_ELEMENT_TYPE + "=" + GenericConstants.PROVISIONING_ID + "&amp;" +    
        GenericConstants.ENDPOINT_KEY + "=" +  END_POINT_NAME + "&amp;" +
        GenericConstants.FQ_ENDPOINT_NAME + "=" +  FQ_END_POINT_NAME + "&amp;" +
        GenericConstants.COMPONENT_NAME + "=" +  RECT_SRV_NAME + "&amp;" +
        GenericConstants.COMPONENT_TNAME + "=" + RECT_TARGET_NAME + "&amp;" +    
        GenericConstants.COMPONENT_TYPE + "=" + RECT_SRV_TYPE + "\" " + 
        " d=\" M"+  TRIANGLE_P1X + "," + TRIANGLE_P1Y + " L" +
        TRIANGLE_P2X + "," + TRIANGLE_P2Y+ " L" +
        TRIANGLE_P3X + "," + TRIANGLE_P3Y + "Z\" fill=\"" +
        COLOR_LIGHT_GRAY +"\" stroke=\"black\" stroke-width=\"1\" > \n";
    
    // the following is a template that represent a connection 
    // between two end points. it contain token that a replaced
    // by the correct value  in the drawConnection(..) method below
    private static final String POLYLINE_ID = "#POLYLINEID#";
    private static final String POLYLINE_SEGMENTS = "#POLYLINESEGMENTS#";
    private static final String POLYLINE_COLOR = "#POLYLINECOLOR#";
    private static final String SVG_CONNECTION_TEMPLATE = 
        "<polyline id=\"" + POLYLINE_ID +  "\" points=\" " +
        POLYLINE_SEGMENTS + "\" stroke-width=\"1\" stroke=\""+
        POLYLINE_COLOR + "\" fill=\"none\" stroke-opacity=\"0.5\"" +
        " shape-rendering=\"crispEdges\" > \n";

    
    // the following is a template that represent the properties 
    // child element of the STD SVG element it is used by the
    // change the element color on mouseover/out
    private static final String PROPS_TYPE = "#PROPS_TYPE#";
    private static final String PROPS_ATTRIB= "#PROPS_ATTRIB#";
    private static final String PROPS_SELECTED = "#PROPSSELECTED#";
    private static final String PROPS_DESELECTED = "#PROPSDESELECTED#";
    private static final String PROPS_TEMPLATE = 
        "<props type=\"" + PROPS_TYPE + "\" attrib=\"" + PROPS_ATTRIB +
        "\" selected=\"" + PROPS_SELECTED + "\" deselected=\"" +
        PROPS_DESELECTED + "\" />";
    
    // The 2 contants used in the tool tip to visully indicate
    // the flow.
    private static final String LEFT2RIGHT_ARROW = " ===&gt; ";
    private static final String RIGHT2LEFT_ARROW = " &lt;=== ";
    
    private static final String[] CONNECTION_COLOR = 
        { COLOR_PURPLE , COLOR_RED , COLOR_GREEN , COLOR_BLUE,COLOR_BLACK};
    
       
    private JBIConnectionsInformation connectionsInfo;
    private JBIServiceInformation serviceInfo;
    private int xPosition;
    private int yPosition;
    private int width;
    private int height;
    private int currentServiceUnitIndex;
    // list of all the services unit used in this app.
    List serviceUnits;
    // map of conumes FQ endpoint to their svg representation point
    Map<String,SVGPointType>   consumeStartMap;
    // map of provides FQ endpoint to their svg representation point
    Map<String,SVGPointType>   providesStartMap;
    Map<SVGConnectionSegments,SVGConnectionSegments>  connectionsSegmentMap;
    // flag to indicate the type of SU (bc = true se=false)
    boolean isBindComponent;
   
    /**
     *  represent a single service unit.
     * @param aServiceInfo - the service unit meta data
     * @param aConnection - the connection info. relate to this service unit
     * @param x - the x value of the start point of the rectangle representing
     *            this service unit.
     * @param y - the y value of the start point of the rectangle representing
     *            this service unit.
     * @param aWidth - the width the rectangle representing this service unit.
     * @param aHeight - the height the rectangle representing this service unit.
     * @param isBC - a flag indicate the type of service unit 
     *               true - binding component
     *               false - service engine
     * @param componentIndex - the index to the list of service unit of the 
     *                         same type.
     */
    public SVGServiceUnit(JBIServiceInformation aServiceInfo,
                           JBIConnectionsInformation aConnection,
                           int x, int y , int aWidth, int aHeight,
                           boolean isBC, int componentIndex) {
        isBindComponent = isBC;
        currentServiceUnitIndex = componentIndex;
        connectionsInfo = aConnection;
        serviceInfo = aServiceInfo;
        xPosition = x;
        yPosition = y;
        width = aWidth;
        height = aHeight;
        consumeStartMap = new HashMap<String,SVGPointType> ();
        providesStartMap = new HashMap<String,SVGPointType> ();
        connectionsSegmentMap = 
                new HashMap<SVGConnectionSegments,SVGConnectionSegments>();
        
    }
    /**
     * @param connectionsInfo The connectionsInfo to set.
     */
    public void setConnectionsInfo(JBIConnectionsInformation connection) {
        this.connectionsInfo = connection;
    }

   public JBIServiceInformation getJBIServiceInformation() {
        return serviceInfo;
   }
    
    
    /**
     * @param serviceInfo The serviceInfo to set.
     */
    public void setServiceInfo(JBIServiceInformation serviceInfo) {
        this.serviceInfo = serviceInfo;
    }

    /**
     * @param height The height to set.
     */
    public void setHeight(int height) {
        this.height = height;
    }
    /**
     * @param width The width to set.
     */
    public void setWidth(int width) {
        this.width = width;
    }
    /**
     * @param position The xPosition to set.
     */
    public void setXPosition(int position) {
        xPosition = position;
    }
    /**
     * @param position The yPosition to set.
     */
    public void setYPosition(int position) {
        yPosition = position;
    }
   
    /**
     * @return rectangle object that is a clone of the rectangle 
     * represent this service unit.
     */
    public Rectangle getThisUnitRect() {
        Rectangle rect = new Rectangle(xPosition, yPosition,width,height );
        return rect;
    }
  
    /**
     * @param rect - set a new rectangle for this service unit allowing
     * to modify it location and size after the instance it its type is
     * instantiated.
     */
    public  void setThisUnitRect(Rectangle rect) {
        xPosition = rect.x;
        yPosition = rect.y;
        width = rect.width;
        height  = rect.height;
    }
     
    /**
     * @param height The height to set.
     */
    public void changeHeightBy(int height) {
        this.height += height;
    }
    /**
     * @param width The width to set.
     */
    public void changeWidthBy(int width) {
        this.width = width;
    }
    /**
     * @param position The xPosition to set.
     */
    public void changeXPositionBy(int position) {
        xPosition += position;
    }
    /**
     * @param position change yPosition to set.
     */
    public void changeYPositionBy(int position) {
        yPosition += position;
    }
    
    /**
     * @param serviceUnitMap The serviceUnitMap to set.
     */
    public void setServiceUnitList(List serviceUnits) {
        this.serviceUnits = serviceUnits;
    }

    public int getCurrentServiceUnitIndex() {
        return currentServiceUnitIndex;
    }

    /**
     * add the svg data (service unit rect. and name) to the main buffer
     * @param buffer - the buffer that hold the svg data 
     */
    public void AddService(StringBuffer buffer) {
        addSVGRectangle(buffer,serviceInfo.getComponentName());
        addSVGText(buffer,serviceInfo.getComponentName());
    }
   
    private void addSVGRectangle(StringBuffer buffer,String componentName) {
        //<rect id=name x=\"XXX\" y=\"YYY\" width=\"WWW\" height=\"HHH\" 
        //http://localhost:8080/cam/faces/manager/framework/generic/tabsFrame.jsp?
        // name=BenchmarkBpel&type=SU&cname=com.sun.bpelse-1.0-2&ctype=SE

        String rectElement = RECTANGLE_TEMPLATE;
        ComponentServiceProviderResolver resolver = 
                ComponentServiceProviderResolver.getInstance();
        rectElement = rectElement.replace(RECT_HREF,
                resolver.getProviderUrl(serviceInfo.getComponentName(),
                GenericConstants.SU_TYPE,getServiceTypeString(),
                componentName));
        rectElement = rectElement.replace(RECT_APP_NAME,getAppName());
        rectElement = rectElement.replace(RECT_SRV_NAME,getServiceName());
        rectElement = rectElement.replace(RECT_COMP_NAME,componentName);
        rectElement = rectElement.replace(RECT_SRV_TYPE, 
                 getServiceTypeString());
        rectElement = rectElement.replace(RECT_SU_NAME,
                 serviceInfo.getServiceUnitName());
        rectElement = rectElement.replace(RECT_X_POS,xPosition+"");
       rectElement = rectElement.replace(RECT_X_POS,xPosition+"");
       rectElement = rectElement.replace(RECT_Y_POS,yPosition+"");
       rectElement = rectElement.replace(RECT_WIDTH,width+"");
       rectElement = rectElement.replace(RECT_HIEGHT,height+"");
       rectElement = rectElement.replace(RECT_TARGET_NAME,
               serviceInfo.getTargetName());
       String suStatus = serviceInfo.getStatus();
       if(suStatus.endsWith(Messages.getString("state.Started"))) {
        rectElement = rectElement.replace(RECT_FILL,COLOR_WHITE);
       }else if(suStatus.endsWith(Messages.getString("state.Stopped"))) {
         rectElement = rectElement.replace(RECT_FILL,COLOR_LIGHT_GRAY);
       }else {
         rectElement = rectElement.replace(RECT_FILL,COLOR_DARK_GRAY);
       }
       logger.finest(rectElement);
       buffer.append(rectElement);
    }

    private void addSVGText(StringBuffer buffer,String name) {
        // <g font-family="SansSerif" font-size="11">
        //<text x="84" y="336"> --> name <--- </text> 
        //</g>
        StringBuffer rectBuffer = 
            new StringBuffer("<g font-family=\"SansSerif\" font-size=\"11\">");
        rectBuffer.append("<text  x=\"" + xPosition+"\"");
        rectBuffer.append(" y=\"" + (yPosition+ height + 10)+"\">" + 
                getServiceName() + "</text> </g> \n");
        logger.finest(rectBuffer.toString());
        buffer.append(rectBuffer);
    }
    
    
    
    public void addConnections(StringBuffer buffer) {
        if(!serviceInfo.hasConsumes()) {
            return;
        }
        drawConsumesPoints(buffer);
        connectToProviders(buffer);
    
    }

    
    
    private void drawConsumesPoints(StringBuffer buffer) {
        SVGPointType p = null;
        Map consumes = serviceInfo.getConsumesMap();

        int index = 1;
        for (Iterator iter = consumes.keySet().iterator(); iter.hasNext();) {
            boolean isBC = serviceInfo.isBindComponent();
            if(isBC) {
                p = new SVGPointType(this, xPosition+width,yPosition+
                           (SVGRenderer.CONSUMES_UNIT_HEIGHT_OFFSET*index++));
            } else {   
                p = new SVGPointType(this, xPosition+width,yPosition+
                           (SVGRenderer.CONSUMES_UNIT_HEIGHT_OFFSET*index++));
            }
            p.setBC(isBC);
            String key = (String) iter.next();
            JBIServiceUnitInformation sui = 
                (JBIServiceUnitInformation)consumes.get(key);

            String statsEndPointName = constructFQNEndPointName(sui,true);
            consumeStartMap.put(sui.getEndpointName()+"_"+ 
                            sui.getFullyQualifiedserviceName(),p);
            logger.finest("c-" + sui.toString() + " p=" +p.toString());
            String circleElement = SVG_CIRCLE_TEMPLATE;
            circleElement = circleElement.replace(END_POINT_NAME, sui.getEndpointName());
            circleElement = circleElement.replace(FQ_END_POINT_NAME, statsEndPointName);
            circleElement = circleElement.replace(RECT_SRV_NAME,
                    serviceInfo.getComponentName());
            circleElement = circleElement.replace(RECT_SRV_TYPE, 
                 getServiceTypeString(isBC));
            circleElement = circleElement.replace(RECT_TARGET_NAME,
               serviceInfo.getTargetName());
            circleElement = circleElement.replace(CIRCLE_ID, sui.toString());
            circleElement = circleElement.replace(CIRCLE_X, p.x+"");
            circleElement = circleElement.replace(CIRCLE_Y, p.y+"");
           
            String titleElement = SVG_TITLE_TEMPLATE;
            titleElement = 
                    titleElement.replace(TITLE_VALUE, sui.getEndpointName());
            String propsElement = PROPS_TEMPLATE;
            propsElement = 
                propsElement.replace(PROPS_TYPE, "circle");
            propsElement = 
                propsElement.replace(PROPS_ATTRIB, "fill");
            propsElement = 
                propsElement.replace(PROPS_SELECTED, "purple");
            propsElement = 
                propsElement.replace(PROPS_DESELECTED, "red");
           
            StringBuffer circleBuffer = new StringBuffer(circleElement+
                    titleElement + propsElement + CIRCLE_END_TAG);
            buffer.append(circleBuffer);
        }
    }
    
    private String constructFQNEndPointName(JBIServiceUnitInformation sui,boolean
            isConsume) {
        StringBuffer buffer = new StringBuffer();
        String qServiceName = sui.getFullyQualifiedserviceName();
        // fix it - replace ^ with comma
        qServiceName = qServiceName.replace("^",",");
        buffer.append(qServiceName);
        buffer.append(",");
        buffer.append(sui.getEndpointName());
        buffer.append(",");
        buffer.append(isConsume ? GenericConstants.CONSUMING_ID :
            GenericConstants.PROVISIONING_ID);
        return buffer.toString();
        
    }
    private int drawProvidesPoints(SVGServiceUnit serviceUnit,
    		StringBuffer buffer,String providesEndPoint,
                int providesPointIndex) {
        SVGPointType p = null;
        Map provides = serviceUnit.serviceInfo.getProvidesMap();

        boolean isBC = serviceUnit.serviceInfo.isBindComponent();
        if(isBC) {
            p = new SVGPointType(this, serviceUnit.xPosition,
                serviceUnit.yPosition+
               (SVGRenderer.PROVIDES_UNIT_HEIGHT_OFFSET*providesPointIndex++));
        } else {   
            p = new SVGPointType(this, serviceUnit.xPosition,
               serviceUnit.yPosition+
               (SVGRenderer.PROVIDES_UNIT_HEIGHT_OFFSET*providesPointIndex++));
        }
        p.setBC(isBC);

        JBIServiceUnitInformation sui = 
            (JBIServiceUnitInformation)provides.get(providesEndPoint);

       String statsEndPointName = constructFQNEndPointName(sui,false);
       providesStartMap.put(sui.getEndpointName()+ "_" +
                        sui.getFullyQualifiedserviceName(),p);
        logger.finest("p-" + sui.getEndpointName() + " p=" +p.toString());

           StringBuffer triangleBuffer = new StringBuffer() ; 
        String pathElement = SVG_TRIANGLE_TEMPLATE;

        pathElement = pathElement.replace(END_POINT_NAME, sui.getEndpointName());
        pathElement = pathElement.replace(FQ_END_POINT_NAME, statsEndPointName);
        pathElement = pathElement.replace(RECT_SRV_NAME,
                serviceUnit.serviceInfo.getComponentName());
        pathElement = pathElement.replace(TRIANGLE_ID, sui.toString());
        pathElement = pathElement.replace(RECT_SRV_TYPE, 
                 getServiceTypeString(isBC));
        pathElement = pathElement.replace(RECT_TARGET_NAME,
               serviceInfo.getTargetName());
        pathElement = pathElement.replace(TRIANGLE_P1X, p.x-3+"");
        pathElement = pathElement.replace(TRIANGLE_P1Y, p.y-3+"");
        pathElement = pathElement.replace(TRIANGLE_P2X, p.x+"");
        pathElement = pathElement.replace(TRIANGLE_P2Y, p.y+"");
        pathElement = pathElement.replace(TRIANGLE_P3X, p.x-3+"");
        pathElement = pathElement.replace(TRIANGLE_P3Y, p.y+3+"");

        triangleBuffer.append(pathElement);
        String titleElement = SVG_TITLE_TEMPLATE;
        titleElement = 
                titleElement.replace(TITLE_VALUE, sui.getEndpointName());
        String propsElement = PROPS_TEMPLATE;
        propsElement = 
            propsElement.replace(PROPS_TYPE, "path");
        propsElement = 
            propsElement.replace(PROPS_ATTRIB, "stroke");
        propsElement = 
            propsElement.replace(PROPS_SELECTED, "red");
        propsElement = 
            propsElement.replace(PROPS_DESELECTED, "black");
        triangleBuffer.append(titleElement + propsElement +PATH_END_TAG);
        logger.finest(triangleBuffer.toString());
        buffer.append(triangleBuffer);
        connectionsInfo.addToAssignedprovidesPointMap(providesEndPoint,p);

        return providesPointIndex;

    }

    
    private void connectToProviders(StringBuffer buffer) {
         int index = 1;
         for (Iterator iter = consumeStartMap.keySet().iterator(); 
             iter.hasNext();) {
            String consumerendPoint = (String) iter.next();
            String providerEndPoint = 
              connectionsInfo.getProviderEndPointForConsumerEndPoint(consumerendPoint);
            // locate the service unit instance containing this provider EndPoint
            SVGServiceUnit serviceUnit = 
                    locateProviderSVGServiceUnit(providerEndPoint);
            // the given providerEndPoint already drawn get it location
            // and continue 
            if(connectionsInfo.hasAssignedprovidesPoint(providerEndPoint)){
                Map provides = serviceUnit.serviceInfo.getProvidesMap();
                JBIServiceUnitInformation sui = 
                   (JBIServiceUnitInformation)provides.get(providerEndPoint);
                providesStartMap.put(sui.getEndpointName()+ "_" + 
                    sui.getFullyQualifiedserviceName(),
                    connectionsInfo.getAssignedprovidesPoint(providerEndPoint));
        		continue;
            }
            if(serviceUnit != null) { 
                // target unit found process connectivity to it
            	if(connectionsInfo.hasServiceUnitProvidesIndex(serviceUnit)) {
            		// get the last provides index for this service unit
            	    index = 
                      connectionsInfo.getServiceUnitProvidesIndex(serviceUnit);
            	}
            	// draw the provides point
            	index = drawProvidesPoints(serviceUnit,buffer,providerEndPoint,
                        index);
            	// update the su index map.
            	connectionsInfo.updateServiceUnitProvidesIndex(serviceUnit, 
                        index);
            }
         }
         drawConnections(buffer);
    }
    
    /*
     *  
     */
    private SVGServiceUnit locateProviderSVGServiceUnit(String providerEndPoint) {
        for (Iterator iter = serviceUnits.iterator(); iter.hasNext();) {
            SVGServiceUnit serviceUnit = (SVGServiceUnit) iter.next();
            if(isSVGServiceUnitForProvider(serviceUnit,providerEndPoint)) {
                return serviceUnit;
            }
        }
        return null;
    }
    
    private boolean isSVGServiceUnitForProvider(SVGServiceUnit serviceUnit, 
                    String providerEndPoint) {
        boolean isServiceUnit = false;
        if(!serviceUnit.serviceInfo.hasProvides()) {
            return isServiceUnit;
        }
        if(serviceUnit.serviceInfo.getProvidesMap().containsKey(providerEndPoint)) {
            isServiceUnit = true;
        }
        
        return isServiceUnit;
    }
    
    

    private void   drawConnections(StringBuffer buffer) {
            String[] consumerdsndPoint = connectionsInfo.getConsumersEndPoint();
            for (int index = 0; index < consumerdsndPoint.length; index++) {
                String consumerEndPointName = consumerdsndPoint[index];
                SVGPointType consumesPoint = 
                        (SVGPointType)consumeStartMap.get(consumerEndPointName);
                String providerEndPointName = 
                    connectionsInfo.getProviderEndPointForConsumerEndPoint(consumerEndPointName);
                SVGPointType providesPoint = 
                     (SVGPointType)providesStartMap.get(providerEndPointName);
                // both point need to be valid to draw the link between
                // them
                if(consumesPoint == null || providesPoint == null) {
                    continue;
                }
              drawConnection(buffer,consumesPoint,providesPoint,
                      consumerEndPointName, providerEndPointName);
            }
    }

    private void   drawConnection(StringBuffer buffer, 
                   SVGPointType consumesPoint, SVGPointType providesPoint,
                   String consumerEndPointName, String providerEndPointName) {
        
        String color = calcConnectionColor(consumesPoint, providesPoint);
        String connectionElement = SVG_CONNECTION_TEMPLATE;
        connectionElement = connectionElement.replace(POLYLINE_ID, 
                consumerEndPointName + "$" + providerEndPointName);
        SVGConnectionSegments cs = 
                new SVGConnectionSegments(connectionsSegmentMap);
        cs.addNewConnectionSegments(consumesPoint, providesPoint, 
                        consumerEndPointName, providerEndPointName,
                        new Rectangle(xPosition,yPosition,width,height));
        
        connectionsSegmentMap.put(cs,cs);
        connectionElement = connectionElement.replace(POLYLINE_SEGMENTS, 
                cs.segmentPointsToString());
        connectionElement = connectionElement.replace(POLYLINE_COLOR, color);
        String titleElement = SVG_TITLE_TEMPLATE;
        titleElement = titleElement.replace(TITLE_VALUE,
                getArrow(consumerEndPointName,providerEndPointName));
        String propsElement = PROPS_TEMPLATE;
        propsElement = 
            propsElement.replace(PROPS_TYPE, "polyline");
        propsElement = 
            propsElement.replace(PROPS_ATTRIB, "stroke");
        propsElement = 
            propsElement.replace(PROPS_SELECTED, "black");
        propsElement = 
            propsElement.replace(PROPS_DESELECTED, color);
        StringBuffer connectionLineBuffer =  
            new StringBuffer(connectionElement + titleElement + 
                    propsElement + POLYLINE_END_TAG);
        logger.finest(connectionLineBuffer.toString());
        buffer.append(connectionLineBuffer);
    }

    private  String  calcConnectionColor(SVGPointType consumesPoint, 
            SVGPointType providesPoint) {
        String color = "black";
        if(consumesPoint == null || providesPoint == null) {
            return color;
        }
        if(consumesPoint.isBC && !providesPoint.isBC) {
            return CONNECTION_COLOR[BC2SE];
        } else if(!consumesPoint.isBC && !providesPoint.isBC) {
            return CONNECTION_COLOR[SE2SE];
        } else if(!consumesPoint.isBC && providesPoint.isBC) {
            return CONNECTION_COLOR[SE2BC];
        } else if(consumesPoint.isBC && providesPoint.isBC) {
            return CONNECTION_COLOR[BC2BC];
        } 
        
        return  color;
    }
    
    
    /**
     * @return Returns the connectionsSegmentMap.
     */
    public Map<SVGConnectionSegments,
            SVGConnectionSegments> getConnectionsSegmentMap() {
        return connectionsSegmentMap;
    }
    /**
     * @param connectionsSegmentMap The connectionsSegmentMap to set.
     */
    public void setConnectionsSegmentMap(Map<SVGConnectionSegments,
            SVGConnectionSegments> connectionsSegmentMap) {
        this.connectionsSegmentMap = connectionsSegmentMap;
    }
    
    private String getServiceTypeString (boolean isBC) {
        if(isBC) {
            return GenericConstants.BC_TYPE;
        }
        return GenericConstants.SE_TYPE;
    }

    private String getServiceTypeString () {
        if(isBindComponent) {
            return GenericConstants.BC_TYPE;
        }
        return GenericConstants.SE_TYPE;
    }
    
    private String getArrow(String consumesEndPoint,String providesEndPoint) {
        if(isBindComponent) {
            return consumesEndPoint + LEFT2RIGHT_ARROW + providesEndPoint;
        }
        return providesEndPoint + RIGHT2LEFT_ARROW + consumesEndPoint;
       
   }
    
   private String getAppName() {
        StringTokenizer st = 
                new StringTokenizer(serviceInfo.getServiceUnitName(),"- "); 
        String appName = st.nextToken();
        return appName;

   } 

   private String getServiceName() {
        String serviceName = Util.trimLeft(serviceInfo.getServiceUnitName(),"-");
        return serviceName;
   } 

   
}
