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
 * @(#)DescriptionField.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.Enumeration;
import javax.swing.AbstractButton;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.plaf.metal.MetalBorders;
import javax.swing.text.JTextComponent;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPathConstants;
import com.sun.jbi.component.toolkit.project.model.Expr;
import com.sun.jbi.component.toolkit.project.util.XPathElement;
import com.sun.jbi.component.toolkit.project.view.App.Status;
import com.sun.jbi.component.toolkit.project.view.event.AppEvent;
import com.sun.jbi.component.toolkit.project.view.event.AppEvent.Type;

/**
 * Displays data from and updates underlying {@link XPathElement}.
 * @author Kevan Simpson
 */
public class DescriptionField extends BasePanel implements KeyListener,
                                                           FocusListener {
    public static final int COLS = 40;
    
    public enum Text { field, area, label, list, radio, file }
    
    public enum Edit { 
        clean,      // value consistent with file content on system 
        dirty,      // user currently editing, uncommitted
        modified    // committed change, not persisted to file
    }
    
    JPanel mMainPanel;
    JLabel mLabel;
    JTextField mTextField;
    private JTextArea mTextArea;
    private JRadioButton[] mRadios;
    private ButtonGroup mBtnGroup;
    private Text mType;
    String mName, mDescription;
    Edit mState = null;
    String mPrevValue;
    Expr mExpr;    // determines displayed value
    XPathElement mXPathElement;
    
    /**
     * 
     */
    public DescriptionField(App app, Expr expr, String name, String desc) {
        this(app, expr, name, desc, Text.field);
    }

    /**
     * 
     */
    public DescriptionField(App app, Expr expr, String name, String desc, Text type) {
        super(app, name, desc, type, expr);
    }

    /**
     * 
     */
    public DescriptionField(App app, Expr expr, String name, String desc, String[] items) {
        super(app, name, desc, Text.radio, expr, items);
    }
    
    /** @see java.awt.event.FocusListener#focusGained(java.awt.event.FocusEvent) */
    public void focusGained(FocusEvent e) {
        if (!e.isTemporary()) {
            AppEvent.Type type = null;
            if (e.getComponent() instanceof JTextComponent) {
                JTextComponent comp = (JTextComponent) e.getComponent();
                int contentLength = comp.getDocument().getLength();
                comp.setCaretPosition(contentLength);
                comp.setSelectionStart(0);
                comp.setSelectionEnd(contentLength);
                type = Type.field_entered;
            }
            else if (e.getComponent() instanceof AbstractButton) {
                AbstractButton button = (AbstractButton) e.getComponent();
                Enumeration<AbstractButton> buttons = mBtnGroup.getElements();
                while (buttons.hasMoreElements()) {
                    if (buttons.nextElement() == button) {
                        type = Type.display_source;
                        if (mPrevValue == null) {
                            mPrevValue = mBtnGroup.getSelection().getActionCommand();
                        }
                    }
                }
            }
            if (type != null) {
                getListenerSupport().fireAppEvent(new AppEvent(type, this));
            }
        }
    }

    /** @see java.awt.event.FocusListener#focusLost(java.awt.event.FocusEvent) */
    public void focusLost(FocusEvent e) {
    }

    /** @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent) */
    public void keyPressed(KeyEvent e) {
        // prevents newline from being inserted in text area
        if (e.getKeyChar() == KeyEvent.VK_ENTER) e.consume();
        // Ctrl+Enter will not be consumed as it is a different char
    }

    /** @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent) */
    public void keyReleased(KeyEvent e) {
        boolean commit = false;
        if (e.getKeyCode() == KeyEvent.VK_ENTER) {
            if (mType == Text.area 
                && (e.getModifiers() == KeyEvent.CTRL_MASK
                    || e.getModifiers() == KeyEvent.CTRL_DOWN_MASK)) {
                // intercept to insert newline
                mTextArea.insert(
                        String.valueOf((char) KeyEvent.VK_ENTER), 
                        mTextArea.getCaretPosition());
            }
            else {
                commit = commit();
            }
        }
        
        // if we're not committing and the value has changed
        if (!commit && !mPrevValue.equals(getText())) {
            markDirty();
        }
    }

    /** @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent) */
    public void keyTyped(KeyEvent e) {
    }

    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#handleEvent(com.sun.jbi.component.toolkit.project.view.event.AppEvent) */
    @Override
    public void handleEvent(AppEvent evt) {
        if (evt.getType() == Type.save_complete) {
//            && evt.getOldValue().equals(getXmlObject())) {
            mLabel.setForeground(Status.good.getColor());
        }
    }

    public String getText() {
        switch (mType) {
            case label:
            case field: {
                return mTextField.getText();
            }
            case area:  {
                return mTextArea.getText();
            }
            case radio: {
                for (Enumeration<AbstractButton> btns = mBtnGroup.getElements();
                     btns.hasMoreElements();) {
                    AbstractButton ab = btns.nextElement();
                    if (ab.isSelected()) {
                        return ab.getText();
                    }
                }
                return null;    // no radio selected
            }
//            case list: {
//                return (mList.getSelectedIndex() < 0) 
//                        ? null : String.valueOf(mList.getSelectedValue());
//            }
            default: return null;
        }
    }
    
    public Expr getExpr() {
        return mExpr;
    }
    
    public XPathElement getXPathElement() {
        return mXPathElement;
    }
    
//    public XmlObject<T> getXmlObject() {
//        return mXmlObject;
//    }
    
    public void modifyXPathElement(XPathElement xpath) {
        if (xpath != null) {
            setXPathElement(xpath);
            markDirty();
            commit();
        }
    }

    public void setXPathElement(XPathElement xpath) {
        if (xpath != null) {
            // store to pass to App when updating source
            mXPathElement = xpath;
            mState = Edit.clean;
            QName type = mExpr.getType();
            switch (mType) {
                case label:
                case field:
                case area: {
                    if (type.equals(XPathConstants.STRING)) {
                        setText(mXPathElement.getString(mExpr.getXPath()));
                    }
                    else if (type.equals(XPathConstants.NODE)) {
                        System.out.println("setValue(Node): "+ mExpr.getXPath());
                    }
                    else {
                        System.out.println("setValue(): "+ mExpr.getXPath());
                    }
                    break;
                }
                case radio: {
                    setText(mXPathElement.getString(mExpr.getXPath()));
                    break;
                }
                case list: {    // do nothing... use DescriptionList
                    break;
                }
            }
        }
        else {
            setText();
        }
    }
    
    boolean commit() {
        if (mState == Edit.dirty) {
            mState = Edit.modified;
            // fire event here so new value in field is available
            getListenerSupport().fireAppEvent(
                    new AppEvent(Type.commit, this));
            mLabel.setForeground(Status.info.getColor());
            getApp().showMessages(Status.info, "Modify committed... click 'Save' to persist change(s) to file!");
            return true;
        }
        
        return false;
    }

    Text getType() {
        return mType;
    }

    void markDirty() {
        if (mState != Edit.dirty &&
            (mType == Text.area || mType == Text.field)) {
            getApp().showMessages(Status.info, "Press 'Enter' to commit changes..");
        }
        mState = Edit.dirty;
        mLabel.setForeground(Color.red);
    }

    protected void setText(String... text) {
        if (text != null) {
            String one = (text.length == 0) ? "" : text[0];
            switch (mType) {
                case label:
                case field: {
                    mTextField.setText(one);
                    mPrevValue = one;
                    break;
                }
                case area:  {
                    mTextArea.setText(one);
                    mPrevValue = one;
                    break;
                }
//                case list: {
//                    mPrevValue = null;
//                    mListModel.clear();
//                    for (String str : text) {
//                        mListModel.addElement(str);
//                        if (mPrevValue == null) {
//                            mPrevValue = str;   // TODO is this right?
//                        }
//                    }
//                    mList.setSelectedValue(mPrevValue, true);
//                    break;
//                }
                case radio: {
                    for (Enumeration<AbstractButton> btns = mBtnGroup.getElements();
                         btns.hasMoreElements();) {
                        AbstractButton ab = btns.nextElement();
                        if (ab.getText().equals(one)) {
                            ab.setSelected(true);
                            break;
                        }
                    }
                    break;
                }
            }
        }
    }
    
    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#init(java.lang.Object[]) */
    protected void init(Object... params) {
        mName = String.valueOf(params[0]);
        mDescription = String.valueOf(params[1]);
        mType = (Text) params[2];
        mExpr = (Expr) params[3];
        String[] items = (mType == Text.radio) 
                ? (String[]) params[4] : new String[0];
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        
        mMainPanel = new JPanel(new BorderLayout());
        JPanel fldPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        mLabel = new JLabel(mName, JLabel.RIGHT);
        mLabel.setToolTipText(mDescription);
        fldPanel.add(mLabel);
        
        mTextField = new JTextField(COLS);
        mTextField.setToolTipText(mDescription);
        
        initFields(fldPanel, items);
        
        fldPanel.setToolTipText(mDescription);

        mMainPanel.add(fldPanel, BorderLayout.CENTER);
        add(mMainPanel);
    }
    
    protected void initFields(JPanel fldPanel, String... items) {
        switch (mType) {
            case field: {
                mTextField.addKeyListener(this);
                mTextField.addFocusListener(this);
                fldPanel.add(mTextField);
                break;
            }
            case area:  {
                mTextArea = new JTextArea(2, COLS);
                mTextArea.setToolTipText(mDescription);
                mTextArea.setLineWrap(true);
                mTextArea.setBorder(mTextField.getBorder());
                mTextArea.addKeyListener(this);
                mTextArea.addFocusListener(this);
                fldPanel.add(mTextArea);
                break;
            }
            case label: {
                mTextField.setEditable(false);
                mTextField.setFocusable(true);
                mTextField.addFocusListener(this);
                fldPanel.add(mTextField);
                break;
            }
//            case list: {
//                mListModel = new DefaultListModel();
//                mList = new JList(mListModel);
//                mList.setLayoutOrientation(JList.VERTICAL_WRAP);
//                mList.setFont(mTextField.getFont());
//                mList.setFixedCellWidth(mTextField.getPreferredSize().width);
//                mList.setFixedCellHeight(mTextField.getPreferredSize().height);
//                mList.setBackground(mTextField.getBackground());
//                mList.setToolTipText(mDescription);
//                mList.setVisibleRowCount(App.MESSAGE_DISPLAY_SIZE);
//                mList.setBorder(MetalBorders.getTextFieldBorder());
////                mList.addKeyListener(this);
////                mList.addPropertyChangeListener(getApp());  // may not need
//                fldPanel.add(mList);
//                break;
//            }
            case radio: {
                JPanel pnlRadio = new JPanel(new FlowLayout(FlowLayout.LEFT, 10, 0));
                pnlRadio.setOpaque(true);
                pnlRadio.setBackground(Color.white);
                pnlRadio.setBorder(MetalBorders.getTextFieldBorder());
                int len = items.length;
                mRadios = new JRadioButton[len];
                mBtnGroup = new ButtonGroup();
                for (int i = 0; i < len; i++) {
                    mRadios[i] = new JRadioButton(items[i]);
                    mRadios[i].addFocusListener(this);
                    mRadios[i].addItemListener(new ItemListener() {
                        public void itemStateChanged(ItemEvent e) {
                            JRadioButton btn = (JRadioButton) e.getItem();
                            if (mPrevValue == null) {
                                mPrevValue = btn.getText();
                            }
                            else if (!btn.getText().equals(mPrevValue)) {
                                mPrevValue = btn.getText();
                                DescriptionField.this.getListenerSupport()
                                        .fireAppEvent(new AppEvent(
                                                Type.display_source, 
                                                DescriptionField.this));
                                markDirty();
                                commit();
                            }
                        }
                    });
                    mRadios[i].setOpaque(false);
                    mBtnGroup.add(mRadios[i]);
                    pnlRadio.add(mRadios[i]);
                }
                fldPanel.add(pnlRadio);
                break;
            }
        }
    }
}
