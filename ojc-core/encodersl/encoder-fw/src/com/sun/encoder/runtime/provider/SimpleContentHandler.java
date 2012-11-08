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
 * @(#)SimpleContentHandler.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;

/**
 * A simple content handler used in testing.  If <code>doPrint</code> is set
 * to false, it is an empty content handler.
 *
 * @author Jun Xu
 */
public class SimpleContentHandler implements ContentHandler {

    private final boolean mDoPrint;

    int mIndent = 0;
    boolean mHasChar = false;
    boolean mIsOpen = false;

    public SimpleContentHandler(boolean doPrint) {
        mDoPrint = doPrint;
    }

    public void setDocumentLocator(Locator locator) {

    }

    public void startDocument() throws SAXException {
        if (!mDoPrint) {
            return;
        }
        System.out.print(
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
    }

    public void endDocument() throws SAXException {
        if (!mDoPrint) {
            return;
        }
        System.out.println();
    }

    public void startPrefixMapping(String prefix, String uri)
            throws SAXException {
    }

    public void endPrefixMapping(String prefix)
            throws SAXException {
    }

    public final void startElement(String uri,
            String localName, String qName, Attributes atts)
            throws SAXException {
        if (!mDoPrint) {
            return;
        }
        System.out.println();
        if (mIsOpen) {
            mIndent++;
        }
        printIndent();
        System.out.print("<" + localName
                + " xmlns=\"" + uri + "\"");
        for (int i = 0; i < atts.getLength(); i++) {
            System.out.print(" " + atts.getQName(i));
            System.out.print("=\"");
            System.out.print(atts.getValue(i));
            System.out.print("\"");
        }
        System.out.print(">");
        mHasChar = false;
        mIsOpen = true;
    }

    public final void endElement(String uri, String localName,
            String qName) throws SAXException {
        if (!mDoPrint) {
            return;
        }
        if (mHasChar) {
            System.out.print("</" + localName + ">");
            mHasChar = false;
        } else {
            System.out.println();
            mIndent--;
            printIndent();
            System.out.print("</" + localName + ">");
        }
        mIsOpen = false;
    }

    public final void characters(char[] ch, int start,
            int length) throws SAXException {
        if (!mDoPrint) {
            return;
        }
        String chars = new String(ch, start, length);
        chars = chars.replace("&", "&amp;");
        System.out.print(chars);
        if (chars.length() > 0) {
            mHasChar = true;
        }
    }

    public void ignorableWhitespace(char[] ch, int start,
            int length) throws SAXException {
    }

    public void processingInstruction(String target,
            String data) throws SAXException {
    }

    public void skippedEntity(String name)
            throws SAXException {
    }

    private void printIndent() {
        for (int i = 0; i < mIndent; i++) {
            System.out.print("    ");
        }
    }
}
