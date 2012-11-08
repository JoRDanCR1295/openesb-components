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
 * @(#)Crud.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view.input;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.Map;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPathConstants;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.project.util.XPathElement;
import com.sun.jbi.component.toolkit.project.view.App;
import com.sun.jbi.component.toolkit.project.view.BasePanel;
import com.sun.jbi.component.toolkit.project.view.DescriptionList;

/**
 * 
 * @author Kevan Simpson
 */
public class Crud extends BasePanel implements ActionListener {
    private DescriptionList mList;
    private String mItemName, mInsertXPath;
    private QName mChild;
    private Input[] mInputs;
    private Map<String, JButton> mBtnMap;
    
    /**
     * @param app
     */
    public Crud(App app, DescriptionList list, String itemName, 
                QName child, String insertXPath, Input... inputs) {
        super(app, list, itemName, child, insertXPath, inputs);
    }

    /** @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent) */
    public void actionPerformed(ActionEvent e) {
        handleAction(ActionText.valueOf(e.getActionCommand()));
    }

    protected boolean editItem(XPathElement item) {
        if (mInputs != null && mInputs.length > 0) {
            String text = (item == null) 
                    ? null : item.getElement().getTextContent();
            XPathElement xpel = mList.getXPathElement();
            Element parent = (Element) xpel.evaluate(xpel.getElement(), 
                    mList.getExpr().getXPath() +"/..", XPathConstants.NODE);
            if (parent == null) {   // guess
                parent = xpel.getElement();
            }
            
            for (Input in : mInputs) {
                Object value = in.prompt(text);
                if (value != null) {
                    if (item == null && text == null) {
                        // create new item to be added
                        Document doc = parent.getOwnerDocument();
                        Element newChild = doc.createElementNS(
                                mChild.getNamespaceURI(), mChild.getLocalPart());
                        item = new XPathElement(newChild);
                    }
                    
                    if (!in.updateItem(item, value)) {
                        String msg = (text == null) 
                                ? "Unable to create "+ mItemName +": "+ value
                                : "Unable to update "+ mItemName +" "+ text 
                                        +" with value: "+ value;
                        JOptionPane.showMessageDialog(null, msg, mItemName 
                                +" Update Failed!", JOptionPane.WARNING_MESSAGE);
                        return false;
                    }
                }
                else {  // user cancelled input
                    System.out.println("aborting edit: "+ text);
                    return false;
                }
            }
            
            if (text == null) { // create + insert
                // indicates the node BEFORE which the new child is inserted
                if (Util.isEmpty(mInsertXPath)) {
                    parent.appendChild(item.getElement());
                }
                else {
                    Element sib = (Element) xpel.evaluate(
                            parent, mInsertXPath, XPathConstants.NODE);
                    if (sib == null) {
                        parent.appendChild(item.getElement());
                    }
                    else {
                        sib.getParentNode().insertBefore(item.getElement(), sib);
                    }
                }
            }
            
            return true;
        }
        
        return false;
    }
    
    protected void handleAction(ActionText axn) {
        XPathElement xpel = mList.getSelected();
        if (xpel == null && axn != ActionText.Create) {
            JOptionPane.showMessageDialog(null, 
                    "Please select a "+ mItemName +" to "+ axn.toString(), 
                    "No "+ mItemName +" Selected!", JOptionPane.WARNING_MESSAGE);
            return;
        }

        String text = mList.getText();
        if (axn != ActionText.Create && isReadOnly(text)) {
            // pre-defined value...cannot modify or remove
            JOptionPane.showMessageDialog(null, 
                    "Cannot "+ axn +" a read-only "+ mItemName +"!", 
                    axn +" Failed!", JOptionPane.WARNING_MESSAGE);
            return;
        }

        boolean updateGui = false;
        switch (axn) {
            case Modify: {
                updateGui = editItem(mList.getSelected());
                break;
            }
            case Create: {
                updateGui = editItem(null);
                break;
            }
            case Delete: {
                if (confirmDelete(mItemName, text)) {
                    updateGui = (xpel.getElement().getParentNode()
                            .removeChild(xpel.getElement()) != null);
                }
                break;
            }
        }

        if (updateGui) {
            mList.modifyXPathElement(mList.getXPathElement());
            boolean count = mList.getItemCount() > 0;
            mBtnMap.get(ActionText.Modify.toString()).setEnabled(count);
            mBtnMap.get(ActionText.Delete.toString()).setEnabled(count);
        }
    }
    
    protected boolean isReadOnly(String text) {
        return false;
    }
    
    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#init(java.lang.Object[]) */
    @Override
    protected void init(Object... params) {
        mList = (DescriptionList) params[0];
        mItemName = String.valueOf(params[1]);
        mChild = (QName) params[2];
        mInsertXPath = String.valueOf(params[3]);
        mInputs = (Input[]) params[4];
        mBtnMap = new HashMap<String, JButton>();
        
        BoxLayout boxBtns = new BoxLayout(this, BoxLayout.Y_AXIS);
        setLayout(boxBtns);

        for (ActionText axn : ActionText.values()) {
            String txt = axn.toString();
            JButton btn = new JButton(txt);
            btn.addActionListener(this);
            btn.setAlignmentX(Component.CENTER_ALIGNMENT);
            add(Box.createVerticalStrut(5));
            add(btn);
            mBtnMap.put(txt, btn);
        }
    }
}
