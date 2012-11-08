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
 * Copyright 2007-2008 ZAZ Consulting. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.zaz.ssapi.protocol.vt100;

import com.zaz.ssapi.protocol.common.util.EncodeConvertUtil;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Gary Zheng
 */
/**
 *
 * @author Gary Zheng
 */
public class VT100 implements EhllAPI {

    ProtocolHandlerVT100 handler;

    public void ConnectPresentationSpace(String host, String port) {
        this.handler = new ProtocolHandlerVT100();
        this.handler.connect(host, Integer.valueOf(port).intValue());
    }

    public void DisconnectPresentationSpace() {
        this.handler.disconnect();
    }

    public void CopyStringToField(String data) {
        byte[] aData = EncodeConvertUtil.getASCIIByteFromString(data);
        System.arraycopy(aData, 0, handler.getSession().getDeviceBuffer(), handler.getSession().getBufferAddr(),
                aData.length);
        handler.getSession().setCursorAddr(handler.getSession().getCursorAddr() + aData.length);
        handler.getSession().setOutputAddr(handler.getSession().getOutputAddr() + aData.length);
    //handler.getSession().setModifiedFieldFlag(true);
    }

    public void CopyPresentationSpace() {
        handler.read();
        handler.parseDataStream();
    }

    public void SendPresentationSpace() {
        handler.assembleDataStream();
        handler.write();
    }

    public void BackupPresentationSpace() {
        handler.backupScreen();
    }

    public String GetPresentationSpace(int index) {
        return EncodeConvertUtil.getStringFromASCIIByte(
                handler.getScreenBackup().get(index));
    }

    public String GetSingleValueByKeyword(String ScreenBuffer, String keyword, String separator, int length) {
        String searchString = keyword + separator;
        int keyStart = ScreenBuffer.indexOf(searchString);
        if (keyStart == -1) {
            return "";
        }
        int valueStart = keyStart + searchString.length();
        return ScreenBuffer.substring(valueStart, valueStart + length).trim();
    }

    public List<String> GetMultiValueByKeyword(String ScreenBuffer, String keyword, String separator, String sub_separator, int length) {
        List<String> valueArray = new ArrayList<String>();
        String searchString = keyword + separator;
        int keyStart = ScreenBuffer.indexOf(searchString);
        if (keyStart == -1) {
            return null;
        }
        int row_num = (keyStart + length) / 80;
        int valueStart = 0;
        while (keyStart != -1) {
            valueStart += keyStart + searchString.length();
            String[] value = ScreenBuffer.substring(valueStart, valueStart + length).split(sub_separator);
            for (int i = 0; i < value.length; i++) {
                valueArray.add(value[i].trim());
            }
            String tempBuffer = ScreenBuffer.substring(valueStart, ScreenBuffer.length());
            keyStart = tempBuffer.indexOf(searchString);
            if (keyStart == -1 || (valueStart + keyStart) / 80 != row_num + 1) {
                break;
            }
            row_num += 1;
        }
        return valueArray;
    }

    public String GetChildNodeValue(String ScreenBuffer, String treeNode) {
        int keyStart = ScreenBuffer.indexOf(treeNode);
        if (keyStart == -1) {
            return "";
        }
        int valueStart = (keyStart / 80 + 1) * 80;
        return ScreenBuffer.substring(valueStart, valueStart + 80).trim();
    }

    public String GetRelationTableValue(String ScreenBuffer, String tableHead, int offset, int columnBegin, int columnEnd) {
        int keyStart = ScreenBuffer.indexOf(tableHead);
        if (keyStart == -1) {
            return "";
        }
        int valueStart = (keyStart / 80 + offset) * 80;
        return ScreenBuffer.substring(valueStart + columnBegin, valueStart + columnEnd).trim();
    }

    public String GetMapTableValue(String ScreenBuffer, String tableHead, int offset, String key, String separator, int length) {
        String searchString = key + separator;
        String endstr1 = "                                                                               ";
        int tbStart = ScreenBuffer.indexOf(tableHead);
        if (tbStart == -1) {
            return "";
        }
        int valueStart = (tbStart / 80 + offset) * 80;
        String tempBuffer = ScreenBuffer.substring(valueStart, ScreenBuffer.length());
        int valueEnd = tempBuffer.indexOf(endstr1);
        tempBuffer = tempBuffer.substring(0, valueEnd - 1);
        int keyStart = tempBuffer.indexOf(searchString);
        if (keyStart == -1) {
            return "";
        }
        valueStart += keyStart + searchString.length();
        return ScreenBuffer.substring(valueStart, valueStart + length).trim();
    }

    public List<String> GetListTableValue(String ScreenBuffer, int offset, int rowsOfData, int rowBegin, int rowEnd, String columns) {
        List<String> listValue = new ArrayList<String>();
        String valueBuf = "";
        String[] cols = columns.split(",");
        //String tempbuf = ScreenBuffer.substring(0, 1919);
        valueBuf += ScreenBuffer.substring(rowBegin * 80, rowEnd * 80);

        //valueBuf = valueBuf.substring(80 * offset);
        while (valueBuf.length() > 0) {
            for (int i = 0; i < cols.length / 3; i++) {
                int rowIndex = Integer.parseInt(cols[i * 3]);
                listValue.add(valueBuf.substring(rowIndex * 80 + Integer.parseInt(cols[i * 3 + 1]),
                        rowIndex * 80 + Integer.parseInt(cols[i * 3 + 2])).trim());
            }
            if (valueBuf.length() <= 80 * rowsOfData) {
                break;
            }
            valueBuf = valueBuf.substring(rowsOfData * 80);
        }
        return listValue;
    }
}
