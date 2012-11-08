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
 * @(#)MessagePanel.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridLayout;
import javax.swing.BorderFactory;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.border.EtchedBorder;
import com.sun.jbi.component.toolkit.project.view.App.Status;

/**
 * 
 * @author Kevan Simpson
 */
public class MessagePanel extends BasePanel {
    private DefaultListModel mListModel;
    private JList mMsgList;
    private int mDisplaySize;
    
    public static void showMessageDialog(String title, Status status, String... msgs) {
        if (msgs != null) {
            JOptionPane.showMessageDialog(
                    null, new MessagePanel(null, title, status, msgs),
                    "CDK", status == Status.good 
                            ? JOptionPane.INFORMATION_MESSAGE
                            : (status == Status.error ? JOptionPane.ERROR_MESSAGE
                                                      : JOptionPane.WARNING_MESSAGE));
        }
    }

    public MessagePanel(App app, String title, Status status, String... msgs) {
        this(app, title, (msgs == null) 
                ? App.MESSAGE_DISPLAY_SIZE 
                : Math.max(App.MESSAGE_DISPLAY_SIZE, msgs.length));
        addMessages(status, msgs);
    }

    /**
     * @param app
     */
    public MessagePanel(App app, String title, int displaySize) {
        super(app, title, Integer.valueOf(displaySize));
    }

    /** 
     * Returns the displaySize.
     * @return the displaySize. 
     */
    public int getDisplaySize() {
        return mDisplaySize;
    }

    /**
     * Sets the displaySize. 
     * @param displaySize The displaySize to set. */
    public void setDisplaySize(int displaySize) {
        mDisplaySize = displaySize;
    }

    public void addMessages(Status status, String... msgs) {
        if (msgs != null) {
            for (String msg : msgs) {
                if (msg.startsWith("ERROR")) {
                    mListModel.addElement(new MessageItem(Status.error, msg));
                }
                else {
                    mListModel.addElement(new MessageItem(status, msg));
                }
                int index = mListModel.size() - 1;
                mMsgList.setSelectedIndex(index);
                mMsgList.ensureIndexIsVisible(index);
            }
        }
    }

    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#init(java.lang.Object[]) */
    @Override
    protected void init(Object... params) {
        setLayout(new GridLayout());
        
        mListModel = new DefaultListModel();
        mMsgList = new JList(mListModel);
        mMsgList.setCellRenderer(new DefaultListCellRenderer() {
            public Component getListCellRendererComponent(JList list, Object value,
                    int index, boolean isSelected, boolean cellHasFocus) {
                // inherit
                Component comp = super.getListCellRendererComponent(
                        list, value, index, isSelected, cellHasFocus);
                if (value instanceof MessageItem) {
                    MessageItem item = (MessageItem) value;
                    comp.setForeground(item.getStatus().getColor());
                    ((JLabel) comp).setText(item.getMessage());
                }
                return comp;
            }
        });
        
        mMsgList.setVisibleRowCount(Math.min(((Integer) params[1]).intValue(), 20));
        JScrollPane scroll = new JScrollPane(mMsgList, 
                JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, 
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scroll.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createEtchedBorder(EtchedBorder.LOWERED), 
                (String) params[0]));
        add(scroll);
    }
    
    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#fixSize() */
    @Override
    protected void fixSize() {
        super.fixSize();
        /*
        com.sun.jbi.component.toolkit.project.view.MessagePanel
        java.awt.Rectangle[x=5,y=5,width=740,height=121]
        javax.swing.JPanel[,0,0,750x131,invalid,layout=java.awt.GridLayout,alignmentX=0.0,alignmentY=0.0,border=javax.swing.border.EmptyBorder@1bf7b23,flags=9,maximumSize=,minimumSize=,preferredSize=]
        java.awt.Rectangle[x=0,y=0,width=750,height=131]
                 */
        Dimension dim = new Dimension(740, 100);
        this.setMinimumSize(dim);
        this.setPreferredSize(dim);
        this.setMaximumSize(dim);
    }

    private static class MessageItem {
        private Status mStatus;
        private String mMessage;
        
        public MessageItem(Status status, String msg) {
            mStatus = status;
            mMessage = msg;
        }
        
        public Status getStatus() {
            return mStatus;
        }
        
        public String getMessage() {
            return mMessage;
        }

        /** @see java.lang.Object#toString() */
        @Override
        public String toString() {
            return getMessage();
        }
    }
}
