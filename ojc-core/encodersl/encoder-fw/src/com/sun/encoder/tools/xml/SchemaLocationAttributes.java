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
 * @(#)SchemaLocationAttributes.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.tools.xml;

import java.net.URL;
import org.xml.sax.Attributes;

/**
 * An implementation of the SAX Attributes interface that represents the
 * schema location.
 *
 * @author Jun Xu
 */
public final class SchemaLocationAttributes implements Attributes {

    private static final String XSI_NS =
            "http://www.w3.org/2001/XMLSchema-instance";
    private static final String TAG_LOCATION = "schemaLocation";
    private static final String TAG_NONS_LOCATION = "noNamespaceSchemaLocation";
    private final URL mSchemaLocation;
    private final String mTargetNamespace;
    private final String mValue;
    private final int mLength;

    public SchemaLocationAttributes(String targetNamespace,
            URL schemaLocation) {
        mSchemaLocation = schemaLocation;
        mTargetNamespace = targetNamespace;
        if (mSchemaLocation == null
                || !"file".equals(mSchemaLocation.getProtocol())) {
            mValue = null;
            mLength = 0;
        } else {
            if (mTargetNamespace == null || mTargetNamespace.length() == 0) {
                mValue = NormalizePath(mSchemaLocation.getPath());
            } else {
                mValue =
                    mTargetNamespace + " "
                    + NormalizePath(mSchemaLocation.getPath());
            }
            mLength = 1;
        }
    }

    public int getLength() {
        return mLength;
    }

    public String getURI(int index) {
        if (mLength == 0) {
            return null;
        }
        if (index == 0) {
            return XSI_NS;
        }
        return null;
    }

    public String getLocalName(int index) {
        if (mLength == 0) {
            return null;
        }
        if (index == 0) {
            if (mTargetNamespace == null || mTargetNamespace.length() == 0) {
                return TAG_NONS_LOCATION;
            }
            return TAG_LOCATION;
        }
        return null;
    }

    public String getQName(int index) {
        if (mLength == 0) {
            return null;
        }
        if (index == 0) {
            if (mTargetNamespace == null || mTargetNamespace.length() == 0) {
                return "xsi:" + TAG_NONS_LOCATION;
            }
            return "xsi:" + TAG_LOCATION;
        }
        return null;
    }

    public String getType(int index) {
        if (mLength == 0) {
            return null;
        }
        if (index == 0) {
            return "CDATA";
        }
        return null;
    }

    public String getValue(int index) {
        if (mLength == 0) {
            return null;
        }
        if (index == 0) {
            return mValue;
        }
        return null;
    }

    public int getIndex(String uri, String localName) {
        if (XSI_NS.equals(uri)
                && (TAG_LOCATION.equals(localName)
                    || TAG_NONS_LOCATION.equals(localName))) {
            return 0;
        }
        return -1;
    }

    public int getIndex(String qName) {
        if (("xsi:" + TAG_LOCATION).equals(qName)
                || ("xsi:" + TAG_NONS_LOCATION).equals(qName)) {
            return 0;
        }
        return -1;
    }

    public String getType(String uri, String localName) {
        if (XSI_NS.equals(uri)
                && (TAG_LOCATION.equals(localName)
                    || TAG_NONS_LOCATION.equals(localName))) {
            return "CDATA";
        }
        return null;
    }

    public String getType(String qName) {
        if (("xsi:" + TAG_LOCATION).equals(qName)
                || ("xsi:" + TAG_NONS_LOCATION).equals(qName)) {
            return "CDATA";
        }
        return null;
    }

    public String getValue(String uri, String localName) {
        if (XSI_NS.equals(uri)
                && (TAG_LOCATION.equals(localName)
                    || TAG_NONS_LOCATION.equals(localName))) {
            return mValue;
        }
        return null;
    }

    public String getValue(String qName) {
        if (("xsi:" + TAG_LOCATION).equals(qName)
                || ("xsi:" + TAG_NONS_LOCATION).equals(qName)) {
            return mValue;
        }
        return null;
    }
    
    private String NormalizePath(String path) {
        if (path == null || path.length() < 3) {
            return path;
        }
        if ((path.charAt(0) == '/' || path.charAt(0) == '\\')
                && path.charAt(2) == ':') {
            //Looks like a Windows full qualified path
            path = path.substring(1);
        }
        path = path.replaceAll(" ", "%20");
        return path;
    }
}
