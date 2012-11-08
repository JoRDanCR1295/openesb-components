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
 * @(#)CachedQueryParameter.java
 *
 *
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.databasebc.util;

/**
 *
 * @author Alexander Lomov, 2010
 */
public class CachedQueryParameter {

    private String name_;
    private int type_;
    private String typeName_;
    private short precision_;
    private int direction_;
    private String default_;
    private boolean defaultSet_;

    public CachedQueryParameter(String name, int type, String typeName, int direction, short precision) {
        name_ = name;
        type_ = type;
        typeName_ = typeName;
        precision_ = precision;
        direction_ = direction;
        default_ = null;
        defaultSet_ = false;
    }

    public CachedQueryParameter(String name, int type, String typeName, int direction) {
        name_ = name;
        type_ = type;
        typeName_ = typeName;
        precision_ = 0;
        direction_ = direction;
        default_ = null;
        defaultSet_ = false;
    }

    public CachedQueryParameter(String name, int type, String typeName) {
        name_ = name;
        type_ = type;
        typeName_ = typeName;
        precision_ = 0;
        direction_ = 1;
        default_ = null;
        defaultSet_ = false;
    }

    // Overloaded for PreparedStatement
    public CachedQueryParameter(String name, int type) {
        name_ = name;
        type_ = type;
        typeName_ = null;
        precision_ = 0;
        direction_ = 1;
        default_ = null;
        defaultSet_ = false;
    }

    public String getName() {
        return name_;
    }

    public int getType() {
        return type_;
    }

    public String getTypeName() {
        return typeName_;
    }

    public short getPrecision() {
        return precision_;
    }

    public int getDirection() {
        return direction_;
    }

    public String getDefault() {
        return default_;
    }

    public void setDefault(String def) {
        default_ = def;
        defaultSet_ = true;
    }

    public boolean hasDefault() {
        return default_ != null;
    }

    public boolean defaultSet() {
        return defaultSet_;
    }

}
