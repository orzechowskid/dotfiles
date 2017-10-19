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
        "import/named": "error",
        "indent": [ "warn", 4, {
            "SwitchCase": 1
        } ],
        /* https://github.com/facebookincubator/create-react-app/issues/2631 */
        "jsx-a11y/href-no-hash": "off",
        "no-console": [ "error", {
            "allow": [ "warn", "error" ]
        } ],
        "no-extra-semi": "warn",
        "no-trailing-spaces": "warn",
        "no-unused-vars": [ "warn", {
            "ignoreRestSiblings": true
        } ],
        "prefer-arrow-callback": "off",
        "quotes": [ "error", "backtick", {
            "avoidEscape": true
        } ],
        "react/jsx-filename-extension": "off",
        "react/jsx-indent": [ "warn", 4 ],
        "react/jsx-indent-props": [ "warn", 4 ],
        "space-before-function-paren": [ "warn", "never" ]
    }
};
