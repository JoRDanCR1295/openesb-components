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
 * @(#)SourcePane.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.Rectangle;
import java.io.File;
import java.io.IOException;
import java.util.Stack;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.project.util.Selection;
import com.sun.jbi.component.toolkit.project.util.XPathElement;
import com.sun.jbi.component.toolkit.project.view.App.Status;

/**
 * 
 * @author Kevan Simpson
 */
public class SourcePane extends BasePanel /* should be AxisExpr? */ {
    private JTextPane mSourceArea;
    private JLabel mFileLbl, mXPathLbl;

    /**
     * @param app
     * @param params
     */
    public SourcePane(App app, Object... params) {
        super(app, params);
    }

    public String getContent() {
        try {
            StyledDocument doc = mSourceArea.getStyledDocument();
            return doc.getText(0, doc.getLength());
        }
        catch (BadLocationException ble) {
            getApp().showMessages(Status.error, 
                    "Failed to read Source view content: "+ ble.getMessage());
        }

        return null;
    }

//    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#updateValues() */
//    @Override
//    protected boolean updateValues() {
//        if (super.updateValues()) {
//            // TODO clear file cache?
//            return true;
//        }
//        
//        return false;
//    }

    // package access
    void loadSource(DescriptionField fld) {
//        XmlObject<Expr> xo = fld.getXmlObject();
        XPathElement xpath = fld.getXPathElement();
        File xmlFile = xpath.getFile();
        String xmlPath = xmlFile.getAbsolutePath();
        
        try {
            // set content and current highlight
            String content = xpath.toXml(false);
            if (!Util.isEmpty(content)) {
                setContent(content, xmlFile);
                Selection sel = fld.getExpr().getRegex()
                        .find(content, fld.getExpr().getXPath(), fld.getXPathElement());
                int prev = mSourceArea.getCaretPosition(),
                    caret = prev;
                if (sel != null) {
                    replace(sel, getStyle());
                    caret = sel.getOffset();
                    // highlight
                    if (prev < caret) {
                        caret += sel.getLength();   // scroll to end of pattern
                    }
                }
                // show selected xml
                mSourceArea.moveCaretPosition(caret);
                Rectangle r = mSourceArea.modelToView(caret);
                if (r != null) {
                    mSourceArea.scrollRectToVisible(r);
                }
            }
            
            mXPathLbl.setText("Expr:  "+ fld.getExpr().getXPath());
        }
        catch (Exception e) {
            getApp().showMessages(Status.error, 
                    "Failed to load source file '"+ xmlPath +"': "+ e.getMessage());
            setText(Util.toString(e));
        }
    }
    
    private Style getStyle() {
        StyledDocument doc = mSourceArea.getStyledDocument();
        Style cdk = doc.getStyle("CDK");
        if (cdk == null) {
            cdk = doc.addStyle("CDK", null);
//            StyleConstants.setBackground(cdk, Color.blue);
            StyleConstants.setForeground(cdk, Color.blue);
            StyleConstants.setBold(cdk, true);
        }

        return cdk;
    }

    private boolean replace(Selection sel, Style style) {
        try {
            StyledDocument doc = mSourceArea.getStyledDocument();
            int offset = sel.getOffset(), len = sel.getLength();
            // remove current selection
            doc.remove(offset, len);
            // remove highlight
            doc.insertString(offset, sel.getText(), style);

            return true;
        }
        catch (BadLocationException ble) {
            getApp().showMessages(Status.error, 
                    "Failed to replace text in Source view: "+ ble.getMessage());
        }
        
        return false;
    }

    private void setText(String text) {
        try {
            StyledDocument doc = mSourceArea.getStyledDocument();
            doc.remove(0, doc.getLength());
            doc.insertString(0, text, null);
        }
        catch (BadLocationException ble) {
            getApp().showMessages(Status.error, 
                    "Failed to set text: "+ ble.getMessage());
        }
    }
    
    private String setContent(String content, File xmlFile) throws IOException {
        String xmlPath = xmlFile.getAbsolutePath();
        // if no file is loaded or a different file, set all text
        setText(content);
        // show abbreviated path to file
        if (xmlPath.length() > 30) {
            Stack<String> st = new Stack<String>();
            int len = 0;
            File dir = xmlFile;
            while (len < 30 && dir != null) {
                st.push(dir.getName());
                len += dir.getName().length();
                dir = dir.getParentFile();
            }
            StringBuffer buff = new StringBuffer();
            buff.append("... ");
            while (!st.isEmpty()) {
                buff.append(st.pop());
                if (st.size() > 0) buff.append("/");
            }
            mFileLbl.setText("File:  "+ buff.toString());
        }
        else {
            mFileLbl.setText("File:  "+ xmlPath);
        }
        mFileLbl.setToolTipText(xmlPath);
//        mCurrentFile = xmlPath;
        
        return content;
    }
    
    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#init(java.lang.Object[]) */
    @Override
    protected void init(Object... params) {
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createEmptyBorder(5, 0, 0, 0),
                BorderFactory.createTitledBorder("Source")));
        
        JPanel pnlLbls = new JPanel(new GridLayout(1, 2, 20, 0));
        mXPathLbl = new JLabel("Expr:  ", JLabel.LEFT);
        pnlLbls.add(mXPathLbl);
        mFileLbl = new JLabel("File:  ", JLabel.LEFT);
        pnlLbls.add(mFileLbl);
        add(pnlLbls);
        add(Box.createVerticalStrut(2));
        mSourceArea = new JTextPane();
        mSourceArea.setFont(new Font("Dialog", Font.PLAIN, 14));
        mSourceArea.setEditable(false);
        // to size source area...
        JTextArea area = new JTextArea(8, DescriptionField.COLS);
        mSourceArea.setPreferredSize(area.getPreferredSize());
        mSourceArea.setMaximumSize(area.getPreferredSize());
        JScrollPane scroll = new JScrollPane(mSourceArea, 
                JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scroll.setMaximumSize(scroll.getPreferredSize());
        JPanel pnlSrc = new JPanel(new GridLayout());
        pnlSrc.add(scroll);
        add(pnlSrc);
    }

    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#fixSize() */
    @Override
    protected void fixSize() {
        super.fixSize();
        /*
        com.sun.jbi.component.toolkit.project.view.SourcePane
        java.awt.Rectangle[x=0,y=530,width=750,height=195]
        javax.swing.JPanel[,0,131,750x725,invalid,layout=javax.swing.BoxLayout,alignmentX=0.0,alignmentY=0.0,border=,flags=9,maximumSize=,minimumSize=,preferredSize=]
        java.awt.Rectangle[x=0,y=131,width=750,height=725]
                 */
        Dimension dim = new Dimension(750, 200);
        this.setMinimumSize(dim);
        this.setPreferredSize(dim);
        this.setMaximumSize(dim);
    }
    
    
}
