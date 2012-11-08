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
 * @(#)CocoSourceArea.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.model;

/**
 * Represents any of the language-delineated areas of Cobol source lines.
 *
 * @author  Noel Ang
 * @version $Revision: 1.3 $
 */
public class CobolSourceArea {

    private int    mStartCol;
    private int    mEndCol;
    private String mName;

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (mName != null && mName.length() > 0) {
            sb.append(mName).append(", ");
        }
        sb.append("startCol=").append(mStartCol);
        sb.append(", endCol=").append(mEndCol);
        return sb.toString();
    }

    /**
     * Define a CobolSourceArea.
     *
     * @param  name Optional name for area
     * @param  startCol Starting column, counting from 1, that this
     *         area represents
     * @param  endCol Ending column, counting from 1, that this area represents
     * @throws java.lang.IndexOutOfBoundsException if startCol and endCol do
     *          not have the relationship 0 < startCol <= endCol
     */
    protected CobolSourceArea(String name, int startCol, int endCol)
        throws IndexOutOfBoundsException {
    
        if (startCol < 1 || endCol < 1 || startCol > endCol ) {
            throw new IndexOutOfBoundsException();
        }
        mName = name;
        mStartCol = startCol;
        mEndCol   = endCol;
    }
    
    /**
     * Obtain the starting inclusive column boundary of the area.
     *
     * @return Starting column boundary
     */
    public int getStartColumn() {
        return mStartCol;
    }
    
    /**
     * Obtain the ending inclusive column boundary of the area.
     *
     * @return Ending column boundary
     */
    public int getEndColumn() {
        return mEndCol;
    }
    
    /**
     * Indicate whether the area defined by the two specified column coordinates
     * is or is within the bounds of the area.  Note that startCol need not be less
     * than endCol.
     *
     * @param  startCol Inclusive boundary column of the area in consideration
     * @param  endCol   The other inclusive boundary column
     * @return true if the specified space matches the area, or is a region of the
     *         area
     */
    public boolean isInArea(int startCol, int endCol) {
        if (mStartCol <= startCol) {
            if (endCol <= mEndCol) {
                return true;
            }
        }
        return false;
    }
}
