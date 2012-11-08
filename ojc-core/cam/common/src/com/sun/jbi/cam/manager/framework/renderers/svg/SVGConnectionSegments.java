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
 * @(#)SVGConnectionSegments.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.renderers.svg;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/*
 
   this class implement the algorithm that will generate to points of the
   polyline which make up the connection between consumes and provides
   point
   
    
        bind comp.                            service engine                               
        
        
        
        ----------                               ------------ 
        |         |                              |          | 
        |         |                              |          | 
        |   1     |                              |    1     | 
        |         |                              |          | 
        |         |                              |          | 
        |         |                              |          | 
        ----------                               ------------ 
  
 
 
        
        ----------                               ------------ 
        |         |                              |          | 
        |         |                              |          | 
        |    2    |                              |    2     | 
        |         |                              |          | 
        |         |                              |          | 
        |         |                              |          | 
        ----------                               ------------ 
            .                                        .
            .                                        .
            .                                        .
            .                                        .
            .                                        .
            .                                        .
            
 
        The current implementation allow connection between the 
        all the service units except between Binding component to
        itself.
        
        1. SE2SEUP - from service engine 2 to service engine 1 
        2. SE2SESL - from service engine to itslef.
        3. SE2SEDOWN - from service engine 1 to service engine 2
        4. SE2BCUP - from service engine 2 to binding comp. 2
        5. SE2BCSL - from service engine 1 to  binding comp. 1
        6. SE2BCDOWN - from service engine 1 to binding comp. 2
 
        7. BC2SEUP - from binding comp. 2 to service engine 1
        8. BC2SESL - from binding comp. 1 to service engine 1
        9. BC2SEDOWN - from binding comp. 1 to service engine 2
       10. BC2BCUP - from binding comp. 2 to  binding comp. 1
       11. BC2BCDOWN - from binding comp. 1 to service engine 2

       it keeps trace of all the end points of the line segement
       used so far and add offest (as many times as needed)
       if possible overlap may occur.
        

 */


public class SVGConnectionSegments {

    private Logger logger = 
            Logger.getLogger(SVGConnectionSegments.class.getName());
    // The following are the all the option 
    // consume point can connect to provides
    private static final int DRAW_INVALID = 0;
    private static final int DRAW_SE2SEUP = 1;
    private static final int DRAW_SE2SESL = 2; //SELF
    private static final int DRAW_SE2SEDOWN = 3;
    private static final int DRAW_SE2BCUP = 4;
    private static final int DRAW_SE2BCSL = 5; 
    private static final int DRAW_SE2BCDOWN = 6;

    private static final int DRAW_BC2SEUP = 7;
    private static final int DRAW_BC2SESL = 8; 
    private static final int DRAW_BC2SEDOWN = 9;
    private static final int DRAW_BC2BCUP = 10;
    private static final int DRAW_BC2BCDOWN = 11; 

    private static final int ENDPOINT_OFFSET = 3; 
    private static final int LINE_INITIAL_SIZE = 5; 
    public static final int LINE_OFFSET_SIZE = 5; 
    
    private static final int DIRECTION_UP = 0; 
    private static final int DIRECTION_DOWN = 1; 
    private static final int DIRECTION_LEFT_RIGHT = 2; 
    private static final int DIRECTION_RIGHT_LEFT = 3; 

    private static final int LINE_SEGMENT_1 = 1;
    private static final int LINE_SEGMENT_2 = 2; 
    private static final int LINE_SEGMENT_3 = 3;
    private static final int LINE_SEGMENT_4 = 4;
    private static final int LINE_SEGMENT_5 = 5; 
    
    List<SVGPointType> currentSegmentPointsList;
    List<SVGPointType> existingSegmentsPointsList;
 /**
  * 
  */
 public SVGConnectionSegments(Map existingConnectionMap) {
//     connectionSegmentMap = existingConnectionMap;
     existingSegmentsPointsList = new ArrayList<SVGPointType>();
     for (Iterator iter = existingConnectionMap.keySet().iterator(); iter.hasNext();) {
         SVGConnectionSegments connectionSegClass = 
             (SVGConnectionSegments) iter.next();
         existingSegmentsPointsList.addAll(connectionSegClass.getCurrentSegmentsList());
    }
     currentSegmentPointsList = new ArrayList<SVGPointType>();
     
 }
 
 public List<SVGPointType> getCurrentSegmentsList() {
     return currentSegmentPointsList;
 }

 public void addNewConnectionSegments(SVGPointType consumesPoint, 
         SVGPointType providesPoint, String consumerEndPointName, 
         String providerEndPointName,   Rectangle  unitRect) {
     
     int drawingOption = getDrawingOption( unitRect,consumesPoint,providesPoint);
     if(drawingOption == DRAW_INVALID) {
         return;
     }
     
     switch (drawingOption) {
         case DRAW_SE2SEUP:
             drawSE2SEAbove(consumesPoint, providesPoint, unitRect);
             break;
         case DRAW_SE2SESL:
             drawSE2Self(consumesPoint, providesPoint, unitRect);
             break;
         case DRAW_SE2SEDOWN:
             drawSE2SEBelow(consumesPoint, providesPoint, unitRect);
             break;
         case DRAW_SE2BCUP:
             drawSE2BCAbove(consumesPoint, providesPoint, unitRect);
             break;
         case DRAW_SE2BCSL:
             drawSE2BCSameLevel(consumesPoint, providesPoint, unitRect);
             break;
         case DRAW_SE2BCDOWN:
             drawSE2BCBelow(consumesPoint, providesPoint, unitRect);
             break;
         case DRAW_BC2SEUP:
             drawBC2SEAbove(consumesPoint, providesPoint, unitRect);
             break;
         case DRAW_BC2SESL:
             drawBC2SESameLevel(consumesPoint, providesPoint, unitRect);
             break;
         case DRAW_BC2SEDOWN:
             drawBC2SEBelow(consumesPoint, providesPoint, unitRect);
             break;
         case DRAW_BC2BCUP:
             drawBC2BCAbove(consumesPoint, providesPoint, unitRect);
             break;
         case DRAW_BC2BCDOWN:
             drawBC2BCBelow(consumesPoint, providesPoint, unitRect);
             break;
     }

     
     
     
 }

 private int getDrawingOption(Rectangle  unitRect,SVGPointType consumesPoint,
                 SVGPointType providesPoint) {
     if(consumesPoint == null || providesPoint == null) {
         return DRAW_INVALID;
     }
     
     if(consumesPoint.x  > SVGRenderer.SERVICE_SE_UNIT_START.x) {
         // consumes point reside on SE
         if(providesPoint.x >= SVGRenderer.SERVICE_SE_UNIT_START.x) {
             // the provides point reside SE self or other SE
             if((providesPoint.y < consumesPoint.y) && 
                             (providesPoint.y < unitRect.y)) {
                 return DRAW_SE2SEUP;
             } else if((providesPoint.y > consumesPoint.y) && 
                  (providesPoint.y > unitRect.y + unitRect.height)) {
                 return DRAW_SE2SEDOWN;
             } else {
                 return DRAW_SE2SESL;
             }
         } else {
             //  the provides point reside on bc
             // the provides point reside SE self or other SE
             if((providesPoint.y < consumesPoint.y) && 
                             (providesPoint.y < unitRect.y)) {
                 return DRAW_SE2BCUP;
             } else if((providesPoint.y > consumesPoint.y) && 
                  (providesPoint.y > unitRect.y + unitRect.height)) {
                 return DRAW_SE2BCDOWN;
             } else {
                 return DRAW_SE2BCSL;
             }
         }
     } else {
         // consume point is on CB
         if(providesPoint.x > SVGRenderer.SERVICE_BC_UNIT_START.x + 
                         SVGRenderer.SERVICE_UNIT_WIDTH) {
             // the provides point reside on SE 
             if((providesPoint.y < consumesPoint.y) && 
                             (providesPoint.y < unitRect.y)) {
                 return DRAW_BC2SEUP;
             } else if((providesPoint.y > consumesPoint.y) && 
                  (providesPoint.y > unitRect.y + unitRect.height)) {
                 return DRAW_BC2SEDOWN;
             } else {
                 return DRAW_BC2SESL;
             }
         } else {
             //  the provides point reside on bc
             if((providesPoint.y < consumesPoint.y) && 
                             (providesPoint.y < unitRect.y)) {
                 return DRAW_BC2BCUP;
             } else if((providesPoint.y > consumesPoint.y) && 
                  (providesPoint.y > unitRect.y + unitRect.height)) {
                 return DRAW_BC2BCDOWN;
             } 
         }
     }
     return  DRAW_INVALID;
 }
 
 public String segmentPointsToString() {
     StringBuffer segments = new StringBuffer();
     
     for (Iterator iter = currentSegmentPointsList.iterator(); iter.hasNext();) {
         Point currentPoint = (Point) iter.next();
         segments.append(currentPoint.x + ","+ currentPoint.y + " ");
     }
     
     
     return segments.toString();
 }

 private void drawSE2SEAbove(SVGPointType consumesPoint, SVGPointType providesPoint,
                 Rectangle  unitRect) {
     currentSegmentPointsList.add(consumesPoint);
     SVGPointType seg1EndPoint = new SVGPointType(consumesPoint);
     // calc end segment 1
     seg1EndPoint.translate(LINE_INITIAL_SIZE, 0);
     while(isSELinePathInUse(seg1EndPoint,unitRect,DIRECTION_UP,
             providesPoint,LINE_SEGMENT_2,DRAW_SE2SEUP)) {
         seg1EndPoint.translate(LINE_OFFSET_SIZE, 0);
     }
     currentSegmentPointsList.add(seg1EndPoint);
     // calc end segemnt 2
     SVGPointType seg2EndPoint =  new SVGPointType(seg1EndPoint.getSvgSU(),
             seg1EndPoint.x,unitRect.y-LINE_OFFSET_SIZE);
     while(isSELinePathInUse(seg2EndPoint,unitRect,DIRECTION_RIGHT_LEFT,
             providesPoint,LINE_SEGMENT_3,DRAW_SE2SEUP)) {
         seg2EndPoint.translate(0, -LINE_OFFSET_SIZE);
     }
     currentSegmentPointsList.add(seg2EndPoint);
     // calc end segemnt 3
     SVGPointType seg3EndPoint =  new SVGPointType(seg2EndPoint.getSvgSU(),
             unitRect.x-LINE_OFFSET_SIZE,seg2EndPoint.y);
     while(isSELinePathInUse(seg3EndPoint,unitRect,DIRECTION_UP,
             providesPoint,LINE_SEGMENT_4,DRAW_SE2SEUP)) {
         seg3EndPoint.translate(-LINE_OFFSET_SIZE,0);
     }
     currentSegmentPointsList.add(seg3EndPoint);
     // calc end segemnt 4
     SVGPointType seg4EndPoint =  new SVGPointType(seg3EndPoint.getSvgSU(),
             seg3EndPoint.x,providesPoint.y);
     currentSegmentPointsList.add(seg4EndPoint);
     // add last segment point
     addLastEndPoint(providesPoint);
     
     
 }

 private void drawSE2Self(SVGPointType consumesPoint, SVGPointType providesPoint,
                 Rectangle  unitRect) {

     currentSegmentPointsList.add(consumesPoint);
     SVGPointType seg1EndPoint = new SVGPointType(consumesPoint);
     // calc end segment 1
     seg1EndPoint.translate(LINE_INITIAL_SIZE, 0);
     while(isSELinePathInUse(seg1EndPoint,unitRect,DIRECTION_UP,
             providesPoint,LINE_SEGMENT_2,DRAW_SE2SESL)) {
         seg1EndPoint.translate(LINE_OFFSET_SIZE, 0);
     }
     currentSegmentPointsList.add(seg1EndPoint);
     // calc end segemnt 2
     SVGPointType seg2EndPoint =  new SVGPointType(seg1EndPoint.getSvgSU(),
             seg1EndPoint.x,unitRect.y-LINE_OFFSET_SIZE);
     while(isSELinePathInUse(seg2EndPoint,unitRect,DIRECTION_RIGHT_LEFT,
             providesPoint,LINE_SEGMENT_3,DRAW_SE2SESL)) {
         seg2EndPoint.translate(0, -LINE_OFFSET_SIZE);
     }
     currentSegmentPointsList.add(seg2EndPoint);
     // calc end segemnt 3
     SVGPointType seg3EndPoint =  new SVGPointType(seg2EndPoint.getSvgSU(),
             unitRect.x-LINE_OFFSET_SIZE,seg2EndPoint.y);
     while(isSELinePathInUse(seg3EndPoint,unitRect,DIRECTION_DOWN,
             providesPoint,LINE_SEGMENT_4,DRAW_SE2SESL)) {
         seg3EndPoint.translate(-LINE_OFFSET_SIZE,0);
     }
     currentSegmentPointsList.add(seg3EndPoint);
     // calc end segemnt 4
     SVGPointType seg4EndPoint =  new SVGPointType(seg3EndPoint.getSvgSU(),
             seg3EndPoint.x,providesPoint.y);
     currentSegmentPointsList.add(seg4EndPoint);
     // add last segment point
     addLastEndPoint(providesPoint);
     
 }
 
 private void drawSE2SEBelow(SVGPointType consumesPoint, SVGPointType providesPoint,
                 Rectangle  unitRect) {
     currentSegmentPointsList.add(consumesPoint);
     SVGPointType seg1EndPoint = new SVGPointType(consumesPoint);
     // calc end segment 1
     seg1EndPoint.translate(LINE_INITIAL_SIZE, 0);
     while(isSELinePathInUse(seg1EndPoint,unitRect,DIRECTION_DOWN,
             providesPoint,LINE_SEGMENT_2,DRAW_SE2SEDOWN)) {
         seg1EndPoint.translate(LINE_OFFSET_SIZE, 0);
     }
     currentSegmentPointsList.add(seg1EndPoint);
     // calc end segemnt 2
     SVGPointType seg2EndPoint =  new SVGPointType(seg1EndPoint.getSvgSU(),
             seg1EndPoint.x,unitRect.y-LINE_OFFSET_SIZE);
     while(isSELinePathInUse(seg2EndPoint,unitRect,DIRECTION_RIGHT_LEFT,
             providesPoint,LINE_SEGMENT_3,DRAW_SE2SEDOWN)) {
         seg2EndPoint.translate(0, LINE_OFFSET_SIZE);
     }
     currentSegmentPointsList.add(seg2EndPoint);
     // calc end segemnt 3
     SVGPointType seg3EndPoint =  new SVGPointType(seg2EndPoint.getSvgSU(),
             unitRect.x-LINE_OFFSET_SIZE,seg2EndPoint.y);
     while(isSELinePathInUse(seg3EndPoint,unitRect,DIRECTION_DOWN,
             providesPoint,LINE_SEGMENT_4,DRAW_SE2SEDOWN)) {
         seg3EndPoint.translate(-LINE_OFFSET_SIZE,0);
     }
     currentSegmentPointsList.add(seg3EndPoint);
     // calc end segemnt 4
     SVGPointType seg4EndPoint =  new SVGPointType(seg3EndPoint.getSvgSU(),
             seg3EndPoint.x,providesPoint.y);
     currentSegmentPointsList.add(seg4EndPoint);
     // add last segment point
     addLastEndPoint(providesPoint);
     
 }

 private void drawSE2BCAbove(SVGPointType consumesPoint, SVGPointType providesPoint,
                 Rectangle  unitRect) {
     currentSegmentPointsList.add(consumesPoint);
     SVGPointType seg1EndPoint = new SVGPointType(consumesPoint);
     // calc end segment 1
     seg1EndPoint.translate(LINE_INITIAL_SIZE, 0);
     while(isSELinePathInUse(seg1EndPoint,unitRect,DIRECTION_UP,
             providesPoint,LINE_SEGMENT_2,DRAW_SE2BCUP)) {
         seg1EndPoint.translate(LINE_OFFSET_SIZE, 0);
     }
     currentSegmentPointsList.add(seg1EndPoint);
     // calc end segemnt 2
     SVGPointType seg2EndPoint =  new SVGPointType(seg1EndPoint.getSvgSU(),
             seg1EndPoint.x,unitRect.y-LINE_OFFSET_SIZE);
     while(isSELinePathInUse(seg2EndPoint,unitRect,DIRECTION_RIGHT_LEFT,
             providesPoint,LINE_SEGMENT_3,DRAW_SE2BCUP)) {
         seg2EndPoint.translate(0, -LINE_OFFSET_SIZE);
     }
     currentSegmentPointsList.add(seg2EndPoint);
     // calc end segemnt 3
     SVGPointType seg3EndPoint =  new SVGPointType(seg2EndPoint.getSvgSU(),
             SVGRenderer.INITIAL_X_OFFSET-LINE_OFFSET_SIZE,seg2EndPoint.y);
     while(isSELinePathInUse(seg3EndPoint,unitRect,DIRECTION_UP,
             providesPoint,LINE_SEGMENT_4,DRAW_SE2BCUP)) {
         seg3EndPoint.translate(-LINE_OFFSET_SIZE,0);
     }
     currentSegmentPointsList.add(seg3EndPoint);
     // calc end segemnt 4
     SVGPointType seg4EndPoint =  new SVGPointType(seg3EndPoint.getSvgSU(),
             seg3EndPoint.x,providesPoint.y);
     currentSegmentPointsList.add(seg4EndPoint);
     // add last segment point
     addLastEndPoint(providesPoint);
     
     
 }
 
 private void drawSE2BCSameLevel(SVGPointType consumesPoint, SVGPointType providesPoint,
                 Rectangle  unitRect) {
     
     currentSegmentPointsList.add(consumesPoint);
     SVGPointType seg1EndPoint = new SVGPointType(consumesPoint);
     // calc end segment 1
     seg1EndPoint.translate(LINE_INITIAL_SIZE, 0);
     while(isSELinePathInUse(seg1EndPoint,unitRect,DIRECTION_UP,
             providesPoint,LINE_SEGMENT_2,DRAW_SE2BCSL)) {
         seg1EndPoint.translate(LINE_OFFSET_SIZE, 0);
     }
     currentSegmentPointsList.add(seg1EndPoint);
     // calc end segemnt 2
     SVGPointType seg2EndPoint =  new SVGPointType(seg1EndPoint.getSvgSU(),
             seg1EndPoint.x,unitRect.y-LINE_OFFSET_SIZE);
     while(isSELinePathInUse(seg2EndPoint,unitRect,DIRECTION_RIGHT_LEFT,
             providesPoint,LINE_SEGMENT_3,DRAW_SE2BCSL)) {
         seg2EndPoint.translate(0, -LINE_OFFSET_SIZE);
     }
     currentSegmentPointsList.add(seg2EndPoint);
     // calc end segemnt 3
     SVGPointType seg3EndPoint =  new SVGPointType(seg2EndPoint.getSvgSU(),
             SVGRenderer.INITIAL_X_OFFSET-LINE_OFFSET_SIZE,seg2EndPoint.y);
     while(isSELinePathInUse(seg3EndPoint,unitRect,DIRECTION_DOWN,
             providesPoint,LINE_SEGMENT_4,DRAW_SE2BCSL)) {
         seg3EndPoint.translate(-LINE_OFFSET_SIZE,0);
     }
     currentSegmentPointsList.add(seg3EndPoint);
     // calc end segemnt 4
     SVGPointType seg4EndPoint =  new SVGPointType(seg3EndPoint.getSvgSU(),
             seg3EndPoint.x,providesPoint.y);
     currentSegmentPointsList.add(seg4EndPoint);
     // add last segment point
     addLastEndPoint(providesPoint);
     
 }

 private void drawSE2BCBelow(SVGPointType consumesPoint, SVGPointType providesPoint,
                 Rectangle  unitRect) {
     currentSegmentPointsList.add(consumesPoint);
     SVGPointType seg1EndPoint = new SVGPointType(consumesPoint);
     // calc end segment 1
     seg1EndPoint.translate(LINE_INITIAL_SIZE, 0);
     while(isSELinePathInUse(seg1EndPoint,unitRect,DIRECTION_DOWN,
             providesPoint,LINE_SEGMENT_2,DRAW_SE2BCDOWN)) {
         seg1EndPoint.translate(LINE_OFFSET_SIZE, 0);
     }
     currentSegmentPointsList.add(seg1EndPoint);
     // calc end segemnt 2
     SVGPointType seg2EndPoint =  new SVGPointType(seg1EndPoint.getSvgSU(),
             seg1EndPoint.x,unitRect.y-LINE_OFFSET_SIZE);
     while(isSELinePathInUse(seg2EndPoint,unitRect,DIRECTION_RIGHT_LEFT,
             providesPoint,LINE_SEGMENT_3,DRAW_SE2BCDOWN)) {
         seg2EndPoint.translate(0, LINE_OFFSET_SIZE);
     }
     currentSegmentPointsList.add(seg2EndPoint);
     // calc end segemnt 3
     SVGPointType seg3EndPoint =  new SVGPointType(seg2EndPoint.getSvgSU(),
             SVGRenderer.INITIAL_X_OFFSET-LINE_OFFSET_SIZE,seg2EndPoint.y);
     while(isSELinePathInUse(seg3EndPoint,unitRect,DIRECTION_DOWN,
             providesPoint,LINE_SEGMENT_4,DRAW_SE2BCDOWN)) {
         seg3EndPoint.translate(-LINE_OFFSET_SIZE,0);
     }
     currentSegmentPointsList.add(seg3EndPoint);
     // calc end segemnt 4
     SVGPointType seg4EndPoint =  new SVGPointType(seg3EndPoint.getSvgSU(),
             seg3EndPoint.x,providesPoint.y);
     currentSegmentPointsList.add(seg4EndPoint);
     // add last segment point
     addLastEndPoint(providesPoint);
     
 }
 
  
 private void drawBC2SESameLevel(SVGPointType consumesPoint, SVGPointType providesPoint,
                 Rectangle  unitRect) {
     currentSegmentPointsList.add(consumesPoint);
     if(consumesPoint.y == providesPoint.y) {
         SVGPointType lastEndPoint = new SVGPointType(providesPoint);
         lastEndPoint.translate(-ENDPOINT_OFFSET, 0);
         currentSegmentPointsList.add(lastEndPoint);
     }else if (consumesPoint.y < providesPoint.y){
         SVGPointType seg1EndPoint = new SVGPointType(consumesPoint);
         // calc end segment 1
         seg1EndPoint.translate(LINE_INITIAL_SIZE, 0);
         while(isBCLinePathInUse(seg1EndPoint,unitRect,DIRECTION_DOWN,
                 providesPoint,LINE_SEGMENT_2,DRAW_BC2SESL)) {
             seg1EndPoint.translate(LINE_OFFSET_SIZE, 0);
         }
         currentSegmentPointsList.add(seg1EndPoint);
         // calc end segemnt 2
         SVGPointType seg2EndPoint =  new SVGPointType(seg1EndPoint.getSvgSU(),
                 seg1EndPoint.x,providesPoint.y);
         currentSegmentPointsList.add(seg2EndPoint);
         // add last segment point
         SVGPointType lastEndPoint = new SVGPointType(providesPoint);
         lastEndPoint.translate(-ENDPOINT_OFFSET, 0);
         currentSegmentPointsList.add(lastEndPoint);
     } else {
         SVGPointType seg1EndPoint = new SVGPointType(consumesPoint);
         // calc end segment 1
         seg1EndPoint.translate(LINE_INITIAL_SIZE, 0);
         while(isBCLinePathInUse(seg1EndPoint,unitRect,DIRECTION_UP,
                 providesPoint,LINE_SEGMENT_2,DRAW_BC2SESL)) {
             seg1EndPoint.translate(LINE_OFFSET_SIZE, 0);
         }
         currentSegmentPointsList.add(seg1EndPoint);
         // calc end segemnt 2
         SVGPointType seg2EndPoint =  new SVGPointType(seg1EndPoint.getSvgSU(),
                 seg1EndPoint.x,providesPoint.y);
         currentSegmentPointsList.add(seg2EndPoint);
         // add last segment point
         addLastEndPoint(providesPoint);
         
     }
     
  }


 private void drawBC2SEBelow(SVGPointType consumesPoint, SVGPointType providesPoint,
                 Rectangle  unitRect) {
     
     currentSegmentPointsList.add(consumesPoint);
     SVGPointType seg1EndPoint = new SVGPointType(consumesPoint);
     // calc end segment 1
     seg1EndPoint.translate(LINE_INITIAL_SIZE, 0);
     while(isBCLinePathInUse(seg1EndPoint,unitRect,DIRECTION_DOWN,
             providesPoint,LINE_SEGMENT_2,DRAW_BC2SESL)) {
         seg1EndPoint.translate(LINE_OFFSET_SIZE, 0);
     }
     currentSegmentPointsList.add(seg1EndPoint);
     // calc end segemnt 2
     SVGPointType seg2EndPoint =  new SVGPointType(seg1EndPoint.getSvgSU(),
             seg1EndPoint.x,providesPoint.y);
     currentSegmentPointsList.add(seg2EndPoint);
     
     // add last segment point
     addLastEndPoint(providesPoint);
 
 }
 private void drawBC2BCAbove(SVGPointType consumesPoint, SVGPointType providesPoint,
                 Rectangle  unitRect) {
     
     currentSegmentPointsList.add(consumesPoint);
     SVGPointType seg1EndPoint = new SVGPointType(consumesPoint);
     // calc end segment 1
     seg1EndPoint.translate(LINE_INITIAL_SIZE, 0);
     while(isBCLinePathInUse(seg1EndPoint,unitRect,DIRECTION_UP,
             providesPoint,LINE_SEGMENT_2,DRAW_BC2BCUP)) {
         seg1EndPoint.translate(LINE_OFFSET_SIZE, 0);
     }
     currentSegmentPointsList.add(seg1EndPoint);
     // calc end segemnt 2
     SVGPointType seg2EndPoint =  new SVGPointType(seg1EndPoint.getSvgSU(),
             seg1EndPoint.x,unitRect.y-LINE_OFFSET_SIZE);
     while(isBCLinePathInUse(seg2EndPoint,unitRect,DIRECTION_RIGHT_LEFT,
             providesPoint,LINE_SEGMENT_3,DRAW_BC2BCUP)) {
         seg2EndPoint.translate(0, -LINE_OFFSET_SIZE);
     }
     currentSegmentPointsList.add(seg2EndPoint);
     // calc end segemnt 3
     SVGPointType seg3EndPoint =  new SVGPointType(seg2EndPoint.getSvgSU(),
             SVGRenderer.INITIAL_X_OFFSET-LINE_OFFSET_SIZE,seg2EndPoint.y);
     while(isBCLinePathInUse(seg3EndPoint,unitRect,DIRECTION_UP,
             providesPoint,LINE_SEGMENT_4,DRAW_BC2BCUP)) {
         seg3EndPoint.translate(-LINE_OFFSET_SIZE,0);
     }
     currentSegmentPointsList.add(seg3EndPoint);
     // calc end segemnt 4
     SVGPointType seg4EndPoint =  new SVGPointType(seg3EndPoint.getSvgSU(),
             seg3EndPoint.x,providesPoint.y);
     currentSegmentPointsList.add(seg4EndPoint);
     // add last segment point
     addLastEndPoint(providesPoint);

 }
 
 private void drawBC2BCBelow(SVGPointType consumesPoint, SVGPointType providesPoint,
                Rectangle  unitRect) {
     currentSegmentPointsList.add(consumesPoint);
     SVGPointType seg1EndPoint = new SVGPointType(consumesPoint);
     // calc end segment 1
     seg1EndPoint.translate(LINE_INITIAL_SIZE, 0);
     while(isBCLinePathInUse(seg1EndPoint,unitRect,DIRECTION_DOWN,
             providesPoint,LINE_SEGMENT_2,DRAW_BC2BCDOWN)) {
         seg1EndPoint.translate(LINE_OFFSET_SIZE, 0);
     }
     currentSegmentPointsList.add(seg1EndPoint);
     // calc end segemnt 2
     SVGPointType seg2EndPoint =  new SVGPointType(seg1EndPoint.getSvgSU(),
             seg1EndPoint.x,unitRect.y-LINE_OFFSET_SIZE);
     while(isBCLinePathInUse(seg2EndPoint,unitRect,DIRECTION_RIGHT_LEFT,
             providesPoint,LINE_SEGMENT_3,DRAW_BC2BCDOWN)) {
         seg2EndPoint.translate(0, LINE_OFFSET_SIZE);
     }
     currentSegmentPointsList.add(seg2EndPoint);
     // calc end segemnt 3
     SVGPointType seg3EndPoint =  new SVGPointType(seg2EndPoint.getSvgSU(),
             SVGRenderer.INITIAL_X_OFFSET-LINE_OFFSET_SIZE,seg2EndPoint.y);
     while(isBCLinePathInUse(seg3EndPoint,unitRect,DIRECTION_DOWN,
             providesPoint,LINE_SEGMENT_4,DRAW_BC2BCDOWN)) {
         seg3EndPoint.translate(-LINE_OFFSET_SIZE,0);
     }
     currentSegmentPointsList.add(seg3EndPoint);
     // calc end segemnt 4
     SVGPointType seg4EndPoint =  new SVGPointType(seg3EndPoint.getSvgSU(),
             seg3EndPoint.x,providesPoint.y);
     currentSegmentPointsList.add(seg4EndPoint);
     // add last segment point
     addLastEndPoint(providesPoint);
     
    
 }


 private void drawBC2SEAbove(SVGPointType consumesPoint, SVGPointType providesPoint,
                Rectangle  unitRect) {
    
     currentSegmentPointsList.add(consumesPoint);
     SVGPointType seg1EndPoint = new SVGPointType(consumesPoint);
     // calc end segment 1
     seg1EndPoint.translate(LINE_INITIAL_SIZE, 0);
     while(isBCLinePathInUse(seg1EndPoint,unitRect,DIRECTION_UP,
             providesPoint,LINE_SEGMENT_2,DRAW_BC2SEUP)) {
         seg1EndPoint.translate(LINE_OFFSET_SIZE, 0);
     }
     currentSegmentPointsList.add(seg1EndPoint);
     // calc end segemnt 2
     SVGPointType seg2EndPoint =  new SVGPointType(seg1EndPoint.getSvgSU(),
             seg1EndPoint.x,providesPoint.y);
     currentSegmentPointsList.add(seg2EndPoint);
     
     // add last segment point
     addLastEndPoint(providesPoint);
 }
 
 private boolean isSELinePathInUse(SVGPointType cPoint, Rectangle rect, 
         int direction, SVGPointType pPoint,int segNo,int drawType ) {
     boolean result = false;
     
     switch (direction) {
	    case DIRECTION_UP:
	         switch (segNo) {
	            case LINE_SEGMENT_2:
	                 return isSELineSeg2UpInUse(cPoint,rect,pPoint);
	            case  LINE_SEGMENT_4:
	                 return isSELineSeg4UpInUse(cPoint,rect,pPoint);
	         }
	        break;
	    case DIRECTION_DOWN:
	         switch (segNo) {
	            case LINE_SEGMENT_2:
	                 return isSELineSeg2DownInUse(cPoint,rect,pPoint);
	            case  LINE_SEGMENT_4:
	                 return isSELineSeg4DownInUse(cPoint,rect,pPoint);
	         }
	        break;
	    case DIRECTION_LEFT_RIGHT:
	        // when start with SE this case is never used
	        break;
	    case DIRECTION_RIGHT_LEFT:
	        List<SVGPointType> combinedList = new ArrayList<SVGPointType>(existingSegmentsPointsList);
	        combinedList.addAll(currentSegmentPointsList);
	        for (int index = 0; index < combinedList.size(); index++) {
	            SVGPointType pointInUse = 
	                (SVGPointType)combinedList.get(index);
	            if(drawType > DRAW_INVALID && drawType < DRAW_SE2BCUP) {
	                // the line is a SE -> SE line
		            if((cPoint.y == pointInUse.y) && 
		               ((pointInUse.x > rect.x) || 
		               (pointInUse.x > rect.x - 
		                       SVGRenderer.UNIT_HORIZONTAL_GAP_SIZE))) {
		                return true;
		            }
	            } else {
	                // the line is a SE -> BC line
                        if(cPoint.y == pointInUse.y) {
//		            if((cPoint.y == pointInUse.y) && 
//				        ((pointInUse.x < SVGRenderer.INITIAL_X_OFFSET))) {
				                return true;
				            }
	            }
            }
	        
	        break;
    } 
     
     return result;
 }
 
 private boolean isSELineSeg2UpInUse(SVGPointType cPoint, Rectangle rect, 
         SVGPointType pPoint) {
     boolean result = false;
     List<SVGPointType> combinedList = new ArrayList<SVGPointType>(existingSegmentsPointsList);
     combinedList.addAll(currentSegmentPointsList);
     
     for (int index = 0; index < combinedList.size(); index++) {
         Object obj = 
             combinedList.get(index);
         SVGPointType pointInUse = 
             (SVGPointType)combinedList.get(index);
         if((cPoint.x == pointInUse.x) && 
            ((pointInUse.y < rect.y) ||
            (pointInUse.y > rect.y- 
                    SVGRenderer.UNIT_VARTICAL_GAP_SIZE))) {
             return true;
         }
     }
     return result;
    
 }
 
 private boolean isSELineSeg4UpInUse(SVGPointType cPoint, Rectangle rect, 
         SVGPointType pPoint) {
     boolean result = false;
     List<SVGPointType> combinedList = 
             new ArrayList<SVGPointType> (existingSegmentsPointsList);
     combinedList.addAll(currentSegmentPointsList);
     
     for (int index = 0; index < combinedList.size(); index++) {
         SVGPointType pointInUse = 
             (SVGPointType)combinedList.get(index);
         if((cPoint.x == pointInUse.x) && 
            ((pointInUse.y > pPoint.y) ||
             (pointInUse.y < cPoint.y))) {
             return true;
         }
     }
 
     
     return result;
    
 }

 private boolean isSELineSeg2DownInUse(SVGPointType cPoint, Rectangle rect, 
         SVGPointType pPoint) {
     boolean result = false;
     List<SVGPointType>  combinedList = 
             new ArrayList<SVGPointType> (existingSegmentsPointsList);
     combinedList.addAll(currentSegmentPointsList);
     
     for (int index = 0; index < combinedList.size(); index++) {
         SVGPointType pointInUse = 
             (SVGPointType)combinedList.get(index);
         if((cPoint.x == pointInUse.x) && 
            ((pointInUse.y > rect.y + rect.height) ||
            (pointInUse.y < rect.y+ rect.height +
                    SVGRenderer.UNIT_VARTICAL_GAP_SIZE))) {
             return true;
         }
     }
     return result;
    
 }
 private boolean isSELineSeg4DownInUse(SVGPointType cPoint, Rectangle rect, 
         SVGPointType pPoint) {
     boolean result = false;
     List<SVGPointType>  combinedList = 
             new ArrayList<SVGPointType> (existingSegmentsPointsList);
     combinedList.addAll(currentSegmentPointsList);
     
     for (int index = 0; index < combinedList.size(); index++) {
         SVGPointType pointInUse = 
             (SVGPointType)combinedList.get(index);
         if((cPoint.x == pointInUse.x) && 
            ((pointInUse.y < pPoint.y) || 
             (pointInUse.y > cPoint.y))) {
             return true;
         }
     }
 
     
     return result;
    
 }
 
 private boolean isBCLinePathInUse(SVGPointType cPoint, Rectangle rect, 
         int direction, SVGPointType pPoint,int segNo,int drawType ) {
     boolean result = false;
     
     switch (direction) {
	    case DIRECTION_UP:
	         switch (segNo) {
	            case LINE_SEGMENT_2:
	                 return isBCLineSeg2UpInUse(cPoint,rect,pPoint,drawType);
	            case  LINE_SEGMENT_4:
	                 return isBCLineSeg4UpInUse(cPoint,rect,pPoint,drawType);
	         }
	        break;
	    case DIRECTION_DOWN:
	         switch (segNo) {
	            case LINE_SEGMENT_2:
	                 return isBCLineSeg2DownInUse(cPoint,rect,pPoint,drawType);
	            case  LINE_SEGMENT_4:
	                 return isBCLineSeg4DownInUse(cPoint,rect,pPoint,drawType);
	         }
	        break;
	    case DIRECTION_LEFT_RIGHT:
	        // when start with SE this case is never used
	        break;
	    case DIRECTION_RIGHT_LEFT:
	        List<SVGPointType>  combinedList = 
                        new ArrayList<SVGPointType> (existingSegmentsPointsList);
	        combinedList.addAll(currentSegmentPointsList);
	        for (int index = 0; index < combinedList.size(); index++) {
	            SVGPointType pointInUse = 
	                (SVGPointType)combinedList.get(index);
	                // the line is a BC -> BC line
		            if((cPoint.y == pointInUse.y) && 
				        ((pointInUse.x < SVGRenderer.INITIAL_X_OFFSET))) {
				                return true;
				            }
            }
	        
	        break;
    } 
     
     return result;
 }

 private boolean isBCLineSeg2UpInUse(SVGPointType cPoint, Rectangle rect, 
         SVGPointType pPoint,int drawType ) {
     boolean result = false;
     List<SVGPointType>  combinedList = 
             new ArrayList<SVGPointType> (existingSegmentsPointsList);
     combinedList.addAll(currentSegmentPointsList);
     
     for (int index = 0; index < combinedList.size(); index++) {
         SVGPointType pointInUse = 
             (SVGPointType)combinedList.get(index);
         switch (drawType) {
          case DRAW_BC2SESL:
	         if((cPoint.x == pointInUse.x) && 
	            ((pointInUse.y < cPoint.y) || 
	            (pointInUse.y >= cPoint.y- LINE_OFFSET_SIZE))) {
	             return true;
	         }
           break;
          case DRAW_BC2SEUP:
 	         if((cPoint.x == pointInUse.x) && 
 	            ((pointInUse.y < cPoint.y) || 
 	            (pointInUse.y > pPoint.y))) {
 	             return true;
 	         }
            break;
          case DRAW_BC2BCUP:
  	         if((cPoint.x == pointInUse.x) && 
  	            ((pointInUse.y < rect.y) || 
  	            (pointInUse.y > rect.y- 
  	                    SVGRenderer.UNIT_VARTICAL_GAP_SIZE))) {
  	             return true;
  	         }
             break;
           
         }
     }
     return result;
    
 }
 
 private boolean isBCLineSeg4UpInUse(SVGPointType cPoint, Rectangle rect, 
         SVGPointType pPoint,int drawType ) {
     boolean result = false;
     List<SVGPointType>  combinedList = 
             new ArrayList<SVGPointType> (existingSegmentsPointsList);
     combinedList.addAll(currentSegmentPointsList);
     
     for (int index = 0; index < combinedList.size(); index++) {
         SVGPointType pointInUse = 
             (SVGPointType)combinedList.get(index);
         if((cPoint.x == pointInUse.x) && 
            ((pointInUse.y > pPoint.y) || 
             (pointInUse.y < cPoint.y))) {
             return true;
         }
     }
     return result;
    
 }

 private boolean isBCLineSeg2DownInUse(SVGPointType cPoint, Rectangle rect, 
         SVGPointType pPoint,int drawType ) {
     boolean result = false;
     List<SVGPointType>  combinedList =
             new ArrayList<SVGPointType> (existingSegmentsPointsList);
     combinedList.addAll(currentSegmentPointsList);
     
     for (int index = 0; index < combinedList.size(); index++) {
         SVGPointType pointInUse = 
             (SVGPointType)combinedList.get(index);
         switch (drawType) {
          case DRAW_BC2SESL:
	         if((cPoint.x == pointInUse.x) && 
	            ((pointInUse.y < cPoint.y) || 
	            (pointInUse.y >= cPoint.y+LINE_OFFSET_SIZE))) {
	             return true;
	         }
           break;
          case DRAW_BC2SEDOWN:
 	         if((cPoint.x == pointInUse.x) && 
 	            ((pointInUse.y > cPoint.y) ||
 	            (pointInUse.y < pPoint.y))) {
 	             return true;
 	         }
            break;
          case DRAW_BC2BCDOWN:
  	         if((cPoint.x == pointInUse.x) && 
  	             ((pointInUse.y > rect.y + rect.height) || 
  	             (pointInUse.y < rect.y+ rect.height +
  	                SVGRenderer.UNIT_VARTICAL_GAP_SIZE))) {

  	             return true;
  	         }
             break;
           
         }
     }

     return result;
    
 }
 private boolean isBCLineSeg4DownInUse(SVGPointType cPoint, Rectangle rect, 
         SVGPointType pPoint,int drawType ) {
     boolean result = false;
     List<SVGPointType>  combinedList = 
             new ArrayList<SVGPointType> (existingSegmentsPointsList);
     combinedList.addAll(currentSegmentPointsList);
     
     for (int index = 0; index < combinedList.size(); index++) {
         SVGPointType pointInUse = 
             (SVGPointType)combinedList.get(index);
         if((cPoint.x == pointInUse.x) && 
            ((pointInUse.y < pPoint.y) || 
             (pointInUse.y > cPoint.y))) {
             return true;
         }
     }
 
     
     return result;
    
 }

 private void addLastEndPoint(SVGPointType providesPoint) {
     SVGPointType lastEndPoint = new SVGPointType(providesPoint);
     lastEndPoint.translate(-ENDPOINT_OFFSET, 0);
     currentSegmentPointsList.add(lastEndPoint);

 }
 
}
