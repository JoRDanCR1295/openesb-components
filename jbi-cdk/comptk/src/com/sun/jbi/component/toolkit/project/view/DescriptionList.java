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
 * @(#)DescriptionList.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view;

import java.awt.BorderLayout;
import java.awt.Component;
import javax.swing.DefaultListModel;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.plaf.metal.MetalBorders;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPathConstants;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import com.sun.jbi.component.toolkit.project.model.Expr;
import com.sun.jbi.component.toolkit.project.util.XPathElement;
import com.sun.jbi.component.toolkit.project.view.input.Crud;
import com.sun.jbi.component.toolkit.project.view.input.Input;

/**
 * 
 * @author Kevan Simpson
 */
public class DescriptionList extends DescriptionField {
    private JList mList;
    private DefaultListModel mListModel;
    private XPathRenderer mRenderer;
    
    /**
     * @param app
     * @param expr
     * @param name
     * @param desc
     */
    public DescriptionList(App app, Expr expr, String name, String desc) {
        super(app, expr, name, desc, Text.list);
    }

    public int getItemCount() {
        return mListModel.getSize();
    }
    
    public XPathElement getSelected() {
        Object obj = mList.getSelectedValue();
        return (obj instanceof XPathElement) ? (XPathElement) obj : null;
    }
    
    /** @see com.sun.jbi.component.toolkit.project.view.DescriptionField#getText() */
    @Override
    public String getText() {
        return mRenderer.render(getSelected());
//        return (mList.getSelectedIndex() < 0) 
//                ? null : String.valueOf(mList.getSelectedValue());
    }

    public void setCrud(QName child, String insert, Input... inputs) {
        Crud crud = new Crud(getApp(), this, mLabel.getText(), child, insert, inputs);
        setCrud(crud);
    }

    public void setCrud(Crud crud) {
        mMainPanel.add(crud, BorderLayout.EAST);
    }
    
    /** @see com.sun.jbi.component.toolkit.project.view.DescriptionField#setXPathElement(com.sun.jbi.component.toolkit.project.util.XPathElement) */
    @Override
    public void setXPathElement(XPathElement xpath) {
        // store to pass to App when updating source
        super.setXPathElement(xpath);
        updateValues();
        mList.setVisibleRowCount(App.MESSAGE_DISPLAY_SIZE);
    }

    /** @see com.sun.jbi.component.toolkit.project.view.DescriptionField#init(java.lang.Object[]) */
    @Override
    protected void init(Object... params) {
        super.init(params);
    }

    /** @see com.sun.jbi.component.toolkit.project.view.DescriptionField#initFields(javax.swing.JPanel) */
    @Override
    protected void initFields(JPanel fldPanel, String... items) {
        mListModel = new DefaultListModel();
        mList = new JList(mListModel);
        mList.setLayoutOrientation(JList.VERTICAL);
        mList.setFont(mTextField.getFont());
        mList.setFixedCellWidth(mTextField.getPreferredSize().width);
        mList.setFixedCellHeight(mTextField.getPreferredSize().height);
        mList.setBackground(mTextField.getBackground());
        mList.setToolTipText(mDescription);
        mList.setVisibleRowCount(App.MESSAGE_DISPLAY_SIZE);
        mList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        mList.setBorder(MetalBorders.getTextFieldBorder());
        mRenderer = new XPathRenderer("text()");
        mList.setCellRenderer(mRenderer);
//        fldPanel.add(mList);
        JScrollPane scroll = new JScrollPane(mList);
        fldPanel.add(scroll);
    }

    protected void setItems(XPathElement... elems) {
        mListModel.clear();
        if (elems != null) {
            for (XPathElement xpel : elems) {
                mListModel.addElement(xpel);
            }
            if (elems.length > 0) {
                mList.setSelectedValue(elems[0], true);
            }
        }
    }
    
    /** @see com.sun.jbi.component.toolkit.project.view.DescriptionField#setText(java.lang.String[]) */
    @Override
    protected void setText(String... text) {
//        mPrevValue = null;
//        mListModel.clear();
//        for (String str : text) {
//            mListModel.addElement(str);
//            if (mPrevValue == null) {
//                mPrevValue = str;   // TODO is this right?
//            }
//        }
//        mList.setSelectedValue(mPrevValue, true);
    }

    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#updateValues() */
    @Override
    protected boolean updateValues() {
        if (super.updateValues() && mXPathElement != null) {
            if (mExpr.getType().equals(XPathConstants.NODESET)) {
                NodeList sls = mXPathElement.getNodeSet(mExpr.getXPath()); 
                int len = (sls == null) ? 0 : sls.getLength();
                if (len == 0) {
                    setItems();
                }
                else {
                    XPathElement[] xelems = new XPathElement[len];
                    for (int i = 0; i < len; i++) {
                        xelems[i] = new XPathElement((Element) sls.item(i));
                    }
                    setItems(xelems);
                }
            }

            return true;
        }
        
        return false;
    }
    
    public static class XPathRenderer extends JLabel implements ListCellRenderer {
        private String mXPath;
        
        public XPathRenderer(String xpath) {
            mXPath = xpath;
            setOpaque(true);
            setHorizontalAlignment(LEADING);
            setVerticalAlignment(CENTER);
        }
        
        /** @see javax.swing.ListCellRenderer#getListCellRendererComponent(javax.swing.JList, java.lang.Object, int, boolean, boolean) */
        public Component getListCellRendererComponent(JList list, Object value,
                int index, boolean isSelected, boolean cellHasFocus) {
            // selection
            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
            } 
            else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }

            setText(render((XPathElement) value));
            
            return this;
        }
        
        public String render(XPathElement xpel) {
            return (xpel == null) ? "" : xpel.getString(mXPath);
        }
    }
}
