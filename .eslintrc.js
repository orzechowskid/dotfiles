module.exports = {
    "extends": "eslint-config-airbnb",
    "parser": "babel-eslint",
    "plugins": [ "import" ],
    "rules": {
        "array-bracket-spacing": [ "warn", "always", {
            "objectsInArrays": false
        } ],
        "arrow-body-style": [ "warn", "as-needed" ],
        "arrow-parens": [ "warn", "always" ],
        "comma-dangle": [ "warn", "never" ],
        "func-names": "off",
        "function-paren-newline": "off",
        "import/named": "warn",
        "indent": [ "warn", 4, {
            "SwitchCase": 1
        } ],
        /* https://github.com/facebookincubator/create-react-app/issues/2631 */
        "jsx-a11y/href-no-hash": "off",
        "no-console": [ "warn", {
            "allow": [ "info", "warn", "error" ]
        } ],
        "no-extra-semi": "warn",
        "import/no-extraneous-dependencies": [ "warn", {
            "devDependencies": true
        } ],
        "no-plusplus": [ "warn", {
            "allowForLoopAfterthoughts": true
        } ],
        "no-trailing-spaces": "warn",
        "no-underscore-dangle": [ "warn", {
            "allow": [ "_id" ] // mongoDB :|
        } ],
        "no-unused-vars": [ "warn", {
            "ignoreRestSiblings": true
        } ],
        "prefer-arrow-callback": "off",
        // gets annoying when trying to set a property on an object
        "prefer-destructuring": "off",
        "quotes": "off",
        /*        "quotes": [ "error", "backtick", {
           "avoidEscape": true
           } ], */
        "react/jsx-filename-extension": "off",
        "react/jsx-indent": [ "warn", 4 ],
        "react/jsx-indent-props": [ "warn", 4 ],
        "space-before-function-paren": [ "warn", {
            "anonymous": "never",
            "asyncArrow": "always",
            "named": "never"
        } ]
    }
};
