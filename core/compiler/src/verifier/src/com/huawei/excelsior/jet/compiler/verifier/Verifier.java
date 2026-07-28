/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.verifier;


import com.huawei.excelsior.jet.compiler.Environment;
import com.huawei.excelsior.jet.compiler.options.BoolOption$;
import com.huawei.excelsior.jet.compiler.verifier.checking.TypeCheckingVerifier;
import com.huawei.excelsior.jet.compiler.verifier.inference.TypeInferenceVerifier;

public class Verifier extends AbstractVerifier {

    @Override
    public VerificationError verifyClass(VerifiableType type, int cfVersion, Environment env, InfoBuilder builder) {
        VerificationError verifyError;
        if (cfVersion >= CFVersion.MIN_SPLIT_VERIFIER) {
            final boolean fallBackSupported = cfVersion <= CFVersion.MAX_INFERENCE_VERIFIER &&
                    !env.enabled(BoolOption$.NotFailOverToOldVerifier);

            builder.setupForTypeChecking(fallBackSupported);
            verifyError = new TypeCheckingVerifier(env, builder, cfVersion, type).verify();
            final boolean undefinedVerificationResult = builder.finishTypeChecking();

            if (fallBackSupported && (undefinedVerificationResult || errorExistsAndIgnoredOnFallBack(verifyError))) {
                verifyError = new TypeInferenceVerifier(env, builder, cfVersion, type).verify();
            }
        } else {
            verifyError = new TypeInferenceVerifier(env, builder, cfVersion, type).verify();
        }

        return verifyError;
    }

    private static boolean errorExistsAndIgnoredOnFallBack(VerificationError err) {
        if (err == null) {
            return false;
        }
        final VerificationError.ErrorKind errorKind = err.errorKind();
        return errorKind == VerificationError.ErrorKind.CLASSFORMAT_ERROR || errorKind == VerificationError.ErrorKind.VERIFY_ERROR;
    }

}
