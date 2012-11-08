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
 * EncodingStyle.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.gestalt.jbi.encoding;

/**
 * The encoding style to use when wanting to
 * explicity define which style to use.
 * Use in conjunction with the property key
 * EncodingComponent.ENCODE_PROPERTY_NAME.
 * The content type is used to define the
 * content type for the attachment.
 */
public enum EncodingStyle {
    TAG("application/fastinfoset"), ZIP("application/x-gzip");

    private String contentType;

    EncodingStyle(String contentType) {
        this.contentType = contentType;
    }

    public String getContentType() {
        return contentType;
    }
}