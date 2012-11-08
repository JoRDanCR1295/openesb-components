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
 * @(#)SVGPointType.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.renderers.svg;

import java.awt.Point;
import java.util.logging.Logger;


public class SVGPointType extends Point {
    
    private Logger logger = 
            Logger.getLogger(SVGPointType.class.getName());
    /**
     * ref. to the service unit containing this point
     */
    private final SVGServiceUnit mSvgSU;
    
    /**
     * the point repreent a bind component point
     */
    boolean isBC;
    
    /**
     * @param unit - he service unit containing this point
     */
    public SVGPointType(SVGServiceUnit unit) {
        super();
        this.mSvgSU = unit;
    }
    /**
     * @param x - the x value of the point represented by 
     *            this instance  
     * @param y - the y value of the point represented by 
     *            this instance 
     * @param unit the service unit containing this point
     */
    public SVGPointType(SVGServiceUnit unit, int x, int y) {
        super(x, y);
        // TODO Auto-generated constructor stub
        this.mSvgSU = unit;
    }
    /**
     * @param p - a point object
     * @param unit the service unit containing this point
     */
    public SVGPointType(SVGServiceUnit unit, Point p) {
        super(p);
        this.mSvgSU = unit;
    }
    /**
     * @param p - a ref. the the object of the same type
     *             allow to make in place copy of it.
     * @param unit the service unit containing this point
     */
    public SVGPointType(SVGPointType p) {
        super(p.x,p.y);
         this.mSvgSU = p.mSvgSU;
    }
    /**
     * @return Returns the isBC.
     */
    public boolean isBC() {
        return isBC;
    }
    /**
     * @param isBC The isBC to set.
     */
    public void setBC(boolean isBC) {
        this.isBC = isBC;
    }

    
    /**
     * @return Returns the mSvgSU.
     */
    public SVGServiceUnit getSvgSU() {
        return mSvgSU;
    }
}
