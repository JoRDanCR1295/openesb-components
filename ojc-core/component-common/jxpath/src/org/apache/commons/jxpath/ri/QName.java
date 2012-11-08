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
 * @(#)QName.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri;


/**
 * A qualified name: a combination of an optional namespace prefix
 * and an local name.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class QName {
    private String prefix;
    private String name;

    public QName(String qualifiedName) {
        int index = qualifiedName.indexOf(':');
        if (index == -1) {
            prefix = null;
            name = qualifiedName;
        }
        else {
            prefix = qualifiedName.substring(0, index);
            name = qualifiedName.substring(index + 1);
        }
    }

    public QName(String prefix, String localName) {
        this.prefix = prefix;
        this.name = localName;
    }

    public String getPrefix() {
        return prefix;
    }

    public String getName() {
        return name;
    }

    public String toString() {
        if (prefix != null) {
            return prefix + ':' + name;
        }
        return name;
    }

    public int hashCode() {
        return name.hashCode();
    }

    public boolean equals(Object object) {
        if (!(object instanceof QName)) {
            return false;
        }
        if (this == object) {
            return true;
        }
        QName that = (QName) object;
        if (!this.name.equals(that.name)) {
            return false;
        }

        if ((this.prefix == null && that.prefix != null)
            || (this.prefix != null && !this.prefix.equals(that.prefix))) {
            return false;
        }

        return true;
    }
}
