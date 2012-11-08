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
 * Copyright 2007-2008 ZAZ Consulting, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.zaz.ssapi.protocol.vt100;

import java.util.List;

/**
 *
 * @author Gary Zheng
 */
public interface EhllAPI {
    /*
     * establish connection to application host
     */

    public void ConnectPresentationSpace(String host, String port);

    /*
     * disconnect from application host
     */
    public void DisconnectPresentationSpace();
//    public void SendKey();
//    public void Wait();

    public void CopyPresentationSpace();

    public void SendPresentationSpace();

    public void BackupPresentationSpace();

    public String GetPresentationSpace(int index);

    public String GetSingleValueByKeyword(String ScreenBuffer, String keyword, String separator, int length);

    public List<String> GetMultiValueByKeyword(String ScreenBuffer, String keyword, String separator, String sub_separator, int length);

    public String GetChildNodeValue(String ScreenBuffer, String treeNode);

    public String GetRelationTableValue(String ScreenBuffer, String tableHead, int offset, int columnBegin, int columnEnd);

    public String GetMapTableValue(String ScreenBuffer, String tableHead, int offset, String key, String separator, int length);
    
    public List<String> GetListTableValue(String ScreenBuffer, int offset, int rowsOfData, int rowBegin, int rowEnd, String columns);

    public void CopyStringToField(String data);

}
